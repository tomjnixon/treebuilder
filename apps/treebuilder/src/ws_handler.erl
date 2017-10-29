-module(ws_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {subscriptions=[]}).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => infinity}}.

websocket_init(_State) ->
    {ok, #state{}}.

websocket_handle({_Type, Data}, State=#state{subscriptions=Subscriptions}) ->
    case jiffy:decode(Data, [return_maps]) of
        #{<<"type">> := <<"subscribe">>,
          <<"topic">> := Topic} ->
            ebus:sub(self(), {user, Topic}),
            {ok, State#state{subscriptions=[Topic|Subscriptions]}};
        #{<<"type">> := <<"publish">>,
          <<"topic">> := Topic,
          <<"payload64">> := Payload64} ->
            ebus:pub({user, Topic}, {user, Topic, base64:decode(Payload64)}),
            {ok, State};
        #{<<"type">> := <<"unsub_all">>} ->
            [ebus:unsub(self(), {user, Topic}) || Topic <- Subscriptions],
            {ok, State#state{subscriptions=[]}}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({user, Topic, Payload}, State) ->
    Msg = jiffy:encode(#{type => message,
                         topic => Topic,
                         payload64 => base64:encode(Payload)}),
    {reply, {text, Msg}, State}.
