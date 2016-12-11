-module(device_manager_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    device_manager_sup:start_link().

stop(_State) ->
    ok.
