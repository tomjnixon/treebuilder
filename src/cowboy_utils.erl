-module(cowboy_utils).

-export([reply_json/3]).
-export([read_json/1]).

reply_json(StatusCode, Json, Req) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Body = jiffy:encode(Json),
    cowboy_req:reply(StatusCode, Headers, Body, Req).

read_json(Req) ->
    case cowboy_req:body(Req) of
        {ok, Body, Req2} ->
            Json = jiffy:decode(Body),
            {ok, Json, Req2};
        Fail ->
            Fail
    end.
