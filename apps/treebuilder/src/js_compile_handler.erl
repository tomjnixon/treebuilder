-module(js_compile_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"cpp_code">> := CppCode}, State) ->
              {JsCode, Errors} = js_compiler:compile_js(CppCode),
              {ok, #{js_code => JsCode, cpp_code => CppCode, errors => Errors}, State}
      end).
