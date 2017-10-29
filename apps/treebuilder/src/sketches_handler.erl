-module(sketches_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req=#{path_info := Path}, State) ->
    handle(Req, Path, State).

handle(Req, [<<"list_sketches">>], State) ->
    Sketches = sketch_manager:list_sketches(),
    JsonSketches = [{Sketch} || Sketch <- Sketches],
    {ok, Req2} = cowboy_utils:reply_json(200, JsonSketches, Req),
    {ok, Req2, State};

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

handle(Req, [<<"rename">>], State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"old_name">> := OldName, <<"new_name">> := NewName}, State) ->
            case sketch_manager:rename(OldName, NewName) of
                {ok, Sketch} -> {ok, [ok, {Sketch}], State};
                {error, Message} -> {ok, [error, Message], State}
            end
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

handle(Req, [<<"delete">>], State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"name">> := Name}, State) ->
              case sketch_manager:delete(Name) of
                  ok -> {ok, [ok], State};
                  error -> {ok, [error], State}
              end
      end);

handle(Req, [<<"show_sketch">>], State1) ->
    cowboy_utils:json_req_response(Req, State1,
      fun (#{<<"name">> := Name}, State) ->
              case sketch_manager:show_sketch(Name) of
                  ok -> {ok, [ok], State};
                  error -> {ok, [error], State}
              end
      end);

handle(Req, _Path, State) ->
    {ok, Req2} = cowboy_req:reply(404, Req),
    {ok, Req2, State}.
