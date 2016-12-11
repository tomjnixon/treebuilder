-module(pinger).
-behaviour(gen_server).

%% API.
-export([start_link/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          hosts,
          delay
}).

%% API.

-spec start_link(integer(), atom()) -> {ok, pid()}.
start_link(Delay, Hosts) ->
    gen_server:start_link(?MODULE, [Delay, Hosts], []).

%% gen_server.

init([Delay, Hosts]) ->
    State = #state{delay=Delay, hosts=Hosts},
    do_ping(State),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ping, State) ->
    do_ping(State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_ping(#state{hosts=Hosts, delay=Delay}) ->
    [net_adm:ping(Host) || Host <- Hosts],
    timer:send_after(Delay, ping).

