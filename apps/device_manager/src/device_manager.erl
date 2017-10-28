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
        serial_port,
        upload_template
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
handle_call({change_sketch, Num}, _From, State=#state{serial_pid=Pid}) ->
    srly:send(Pid, <<Num:8>>),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({serial, _Pid, _Data}, State) ->
    % io:format("got data: ~p~n", [Data]),
    {noreply, State};
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
