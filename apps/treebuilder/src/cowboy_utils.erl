-module(cowboy_utils).

-export([reply_json/3]).
-export([read_json/1]).
-export([json_req_response/3]).

reply_json(StatusCode, Json, Req) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Body = jiffy:encode(Json),
    cowboy_req:reply(StatusCode, Headers, Body, Req).

read_json(Req) ->
    case cowboy_req:body(Req) of
        {ok, Body, Req2} ->
            Json = jiffy:decode(Body, [return_maps]),
            {ok, Json, Req2};
        Fail ->
            Fail
    end.

json_req_response(Req, State, F) ->
    {ok, Json, Req2} = cowboy_utils:read_json(Req),
    
    {ok, Response, State1} = F(Json, State),
    
    {ok, Req3} = cowboy_utils:reply_json(200, Response, Req2),
    {ok, Req3, State1}.
