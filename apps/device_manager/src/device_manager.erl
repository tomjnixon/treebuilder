-module(device_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([load_hex/1]).
-export([change_sketch/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
        serial_pid=none,
        serial_buf=(<<>>),
        serial_port,
        upload_template,
        subscriptions=[]
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load_hex(Hex) ->
    gen_server:call(device_manager_address(), {load_hex, Hex}, 20000).

change_sketch(Num) ->
    gen_server:call(device_manager_address(), {change_sketch, Num}).

%% gen_server.

init([]) ->
    {ok, UploadTemplateStr} = application:get_env(device_manager, upload_dtl),
    {ok, UploadTemplate} = erlydtl:compile_template(UploadTemplateStr, device_manager_dtl,
                                                    [{out_dir, false}]),
    
    {ok, SerialPort} = application:get_env(device_manager, serial_port),
    
    {ok, ensure_serial_connected(#state{serial_port=SerialPort,
                                        upload_template=UploadTemplate})}.

handle_call({load_hex, Hex}, _From, State=#state{serial_port=SerialPort,
                                                 upload_template=UploadTemplate}) ->
    State1 = ensure_serial_disconnected(State),
    
    HexFile = lib:nonl(os:cmd("mktemp /tmp/XXXXXX.hex")),
    ok = file:write_file(HexFile, Hex),
    
    {ok, CmdIO} = UploadTemplate:render([{serial_port, SerialPort},
                                         {hex_file, HexFile}]),
    Cmd = erlang:binary_to_list(erlang:iolist_to_binary(CmdIO)),
    
    {ok, _} = exec:run(Cmd, [sync, stderr, stdout]),
    
    file:delete(HexFile),
    State2 = ensure_serial_connected(State1),
    {reply, ok, State2};
handle_call({change_sketch, _Num}, _From, State=#state{serial_pid=none}) ->
    {reply, not_connected, State};
handle_call({change_sketch, Num}, _From, State) ->
    NewState = send_change_sketch(Num, State),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({serial, _Pid, Data}, State=#state{serial_buf=Buf}) ->
    {NewBuf, NewState} = handle_serial(<<Buf/binary, Data/binary>>, State),

    {noreply, NewState#state{serial_buf=NewBuf}};
handle_info({user, _Topic, _Payload}, State=#state{serial_pid=none}) ->
    {noreply, State};
handle_info({user, Topic, Payload}, State) ->
    NewState = send_message(Topic, Payload, State),
    {noreply, NewState};
handle_info(Info, State) ->
    error_logger:info_report([unknown_message, {message, Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private.

% Try to connect Ntries times.
ensure_serial_connected(_State, 0) ->
    erlang:error(could_not_connect);
ensure_serial_connected(State=#state{serial_pid=none,
                                     serial_port=SerialPort}, Ntries) ->
    case file:read_file_info(SerialPort) of
        {ok, _} ->
            % Attempt to start the serial port. This may occasionally fail
            % under normal operation, if the serial port disappears between
            % checking that it exists and trying to open it; so handle this
            % 'gracefully'.
            process_flag(trap_exit, true),
            Result = srly:start_link(SerialPort,
                                     application:get_env(device_manager, serial_opts, [])),

            receive % handle possible exit message
                {'EXIT', _, _} -> ok
            after
                10 -> ok
            end,

            process_flag(trap_exit, false),

            case Result of
                {ok, Pid} ->
                    State#state{serial_pid=Pid};
                {error, Error} ->
                    error_logger:error_report([error_opening_serial_port, {error, Error}]),
                    timer:sleep(500),
                    ensure_serial_connected(State, Ntries - 1)
            end;
        {error, _} ->
            timer:sleep(500),
            ensure_serial_connected(State, Ntries - 1)
    end;
ensure_serial_connected(State=#state{serial_pid=_Pid}, _Ntries) -> State.

% Make sure we're connected.
ensure_serial_connected(State) -> ensure_serial_connected(State, 10).

% Make sure we're disconnected.
ensure_serial_disconnected(State=#state{serial_pid=none}) -> State;
ensure_serial_disconnected(State=#state{serial_pid=Pid}) ->
    ok = srly:close(Pid),
    State#state{serial_pid=none}.

% Get the address of the device manager
device_manager_address() ->
    {?MODULE, application:get_env(device_manager, device_manager_node, node())}.

% serial protocol handling
% messages start with 0x7e and end with 0x7f.
% special characters (7e, 7f, 7d) are escaped by the escape character 0x7d
% followed by the character xored with 0x20.

encapsulate(Message) ->
    <<16#7e:8, (escape(Message))/binary, 16#7f:8>>.

escape(Buf) -> escape(Buf, <<>>).
escape(<<>>, Acc) ->
    Acc;
escape(<<16#7d:8, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, 16#7d, 16#5d>>);
escape(<<16#7e:8, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, 16#7d, 16#5e>>);
escape(<<16#7f:8, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, 16#7d, 16#5f>>);
escape(<<B:8, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, B>>).

unescape(Buf) -> unescape(Buf, <<>>).
unescape(<<>>, Acc) ->
    Acc;
unescape(<<16#7d:8, B:8, Rest/binary>>, Acc) ->
    unescape(Rest, <<Acc/binary, (B bxor 16#20):8>>);
unescape(<<B:8, Rest/binary>>, Acc) ->
    unescape(Rest, <<Acc/binary, B:8>>).

handle_serial(<<>>, State) -> {<<>>, State};
handle_serial(Buf=(<<16#7e:8, Tail/binary>>), State) ->
    case binary:split(Tail, <<16#7f>>) of
        [Message, NewTail] ->
            NewState = handle_message(unescape(Message), State),
            handle_serial(NewTail, NewState);
        [_IncompleteMessage] -> {Buf, State}
    end;
handle_serial(<<_Junk:8, Tail/binary>>, State) -> handle_serial(Tail, State).

% serial messages

-define(SEND_SWITCH, 0).
-define(SEND_MESSAGE, 1).

-define(RECV_SWITCHED, 0).
-define(RECV_PUBLISH, 1).
-define(RECV_UNSUB_ALL, 2).
-define(RECV_SUBSCRIBE, 3).

handle_message(<<?RECV_SWITCHED, _Sketch:16>>, State) ->
    State;
handle_message(<<?RECV_PUBLISH, Data/binary>>, State) ->
    <<TopicLen:8, Topic:TopicLen/binary,
      PayloadLen:16, Payload:PayloadLen/binary>> = Data,
    ebus:pub({user, Topic}, {user, Topic, Payload}),
    State;
handle_message(<<?RECV_UNSUB_ALL>>, State=#state{subscriptions=Subscriptions}) ->
    [ebus:unsub(self(), {user, Topic}) || Topic <- Subscriptions],
    State#state{subscriptions=[]};
handle_message(<<?RECV_SUBSCRIBE, Data/binary>>, State=#state{subscriptions=Subscriptions}) ->
    <<TopicLen:8, Topic:TopicLen/binary>> = Data,
    ebus:sub(self(), {user, Topic}),
    State#state{subscriptions=[Topic | Subscriptions]}.

send_change_sketch(Num, State=#state{serial_pid=Pid}) ->
    srly:send(Pid, encapsulate(<<?SEND_SWITCH, Num:16>>)),
    State.

send_message(Topic, Payload, State=#state{serial_pid=Pid}) ->
    Message = <<?SEND_MESSAGE,
                (erlang:size(Topic)):8, Topic/binary,
                (erlang:size(Payload)):16, Payload/binary>>,
    srly:send(Pid, encapsulate(Message)),
    State.
