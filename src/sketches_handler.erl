-module(sketches_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, [<<"get_state">>], State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"name">> := Name}, State) ->
              case sketch_manager:get_sketch_state(Name) of
                  null -> {ok, [ok, null], State};
                  Sketch -> {ok, [ok, {Sketch}], State}
              end
      end);

handle(Req, [<<"save">>], State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"name">> := Name, <<"cpp_code">> := CppCode}, State) ->
              {ok, Sketch} = sketch_manager:save(Name, CppCode),
              % io:format("RET: ~p~n", [Sketch]),
              {ok, [ok, {Sketch}], State}
      end);

handle(Req, [<<"enable">>], State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"name">> := Name}, State) ->
              case sketch_manager:enable(Name) of
                  {ok, Sketch} -> {ok, [ok, {Sketch}], State};
                  {error, Error} -> {ok, [error, Error], State}
              end
      end);

handle(Req, [<<"disable">>], State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"name">> := Name}, State) ->
              case sketch_manager:disable(Name) of
                  {ok, Sketch} -> {ok, [ok, {Sketch}], State};
                  {error, Error} -> {ok, [error, Error], State}
              end
      end);

handle(Req, _, State) ->
    {ok, Req2} = cowboy_req:reply(404, "Not Found", Req),
    {ok, Req2, State}.

handle(Req, State=#state{}) ->
    {PathInfo, Req2} = cowboy_req:path_info(Req),
    handle(Req2, PathInfo, State).
    

terminate(_Reason, _Req, _State) ->
    ok.
