-module(device_manager_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{device_manager,
              {device_manager, start_link, []},
              permanent, 5000, worker, [device_manager]}
            ],
    {ok, {{one_for_one, 1, 5}, Procs}}.
