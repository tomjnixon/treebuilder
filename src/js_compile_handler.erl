-module(js_compile_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Json, Req2} = cowboy_utils:read_json(Req),
    
    #{<<"cpp_code">> := CppCode} = Json,
    {JsCode, Errors} = js_compiler:compile_js(CppCode),
    
    {ok, Req3} = cowboy_utils:reply_json(200, #{js_code => JsCode, errors => Errors}, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
