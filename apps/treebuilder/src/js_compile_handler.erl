-module(js_compile_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"cpp_code">> := CppCode}, State) ->
              {JsCode, Errors} = js_compiler:compile_js(CppCode),
              {ok, #{js_code => JsCode, cpp_code => CppCode, errors => Errors}, State}
      end).

terminate(_Reason, _Req, _State) ->
    ok.
