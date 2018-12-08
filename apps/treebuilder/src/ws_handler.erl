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

error_resp(Message) ->
    {reply, {close, 3000, Message}, closed}.

handle_json_msg(Message, State=#state{subscriptions=Subscriptions}) ->
    case Message of
        #{<<"type">> := <<"subscribe">>,
          <<"topic">> := Topic}
          when is_binary(Topic) ->
            ok = ebus:sub(self(), {user, Topic}),
            {ok, State#state{subscriptions=[Topic|Subscriptions]}};
        #{<<"type">> := <<"subscribe">>} ->
            error_resp(<<"missing or incorrect topic">>);

        #{<<"type">> := <<"publish">>,
          <<"topic">> := Topic,
          <<"payload64">> := Payload64}
          when is_binary(Topic), is_binary(Payload64) ->
            try base64:decode(Payload64) of
                Payload ->
                    ok = ebus:pub({user, Topic}, {user, Topic, Payload}),
                    {ok, State}
            catch
                error:_ -> error_resp("could not parse payload64")
            end;
        #{<<"type">> := <<"publish">>} ->
            error_resp(<<"missing or incorrect topic and/or payload64">>);

        #{<<"type">> := <<"unsub_all">>} ->
            [ok = ebus:unsub(self(), {user, Topic}) || Topic <- Subscriptions],
            {ok, State#state{subscriptions=[]}};

        #{<<"type">> := _} ->
            error_resp(<<"unknown type">>);
        #{} ->
            error_resp(<<"missing type">>);
        _ ->
            error_resp(<<"messages must be objects">>)
    end.

websocket_handle({_Type, Data}, State) ->
    try jiffy:decode(Data, [return_maps]) of
        Message -> handle_json_msg(Message, State)
    catch
        {error, {Char, ErrorAtom}} ->
            Error = erlang:iolist_to_binary(
                      io_lib:format(
                        "json error at char ~p: ~p~n",
                        [Char, ErrorAtom])),
            error_resp(Error)
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({user, Topic, Payload}, State) ->
    Msg = jiffy:encode(#{type => message,
                         topic => Topic,
                         payload64 => base64:encode(Payload)}),
    {reply, {text, Msg}, State}.
