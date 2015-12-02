-module(device_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([load_hex/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
        serial_pid=none
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load_hex(Hex) ->
    gen_server:call(?MODULE, {load_hex, Hex}).

%% gen_server.

init([]) ->
    {ok, ensure_serial_connected(#state{})}.

handle_call({load_hex, Hex}, _From, State) ->
    State1 = ensure_serial_disconnected(State),
    
    HexFile = lib:nonl(os:cmd("mktemp /tmp/XXXXXX.hex")),
    ok = file:write_file(HexFile, Hex),
    
    {ok, _} = exec:run([os:find_executable("teensy-loader-cli"), "-v", "-s", "--mcu=mk20dx256", HexFile],
                       [sync]),
    
    file:delete(HexFile),
    State2 = ensure_serial_connected(State1),
    {reply, ok, State2};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({serial, _Pid, _Data}, State) ->
    % io:format("got data: ~p~n", [Data]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private.
%% 
ensure_serial_connected(State=#state{serial_pid=none}) ->
    Fname = "/dev/ttyACM0",
    case file:read_file_info(Fname) of
        {ok, _} ->
            {ok, Pid} = srly:start_link(Fname, []),
            State#state{serial_pid=Pid};
        {error, _} ->
            timer:sleep(500),
            ensure_serial_connected(State)
    end;
ensure_serial_connected(State=#state{serial_pid=_Pid}) -> State.

ensure_serial_disconnected(State=#state{serial_pid=none}) -> State;
ensure_serial_disconnected(State=#state{serial_pid=Pid}) ->
    ok = srly:close(Pid),
    State#state{serial_pid=none}.

