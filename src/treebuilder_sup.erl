-module(treebuilder_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{js_compiler,
              {js_compiler, start_link, []},
              permanent, 5000, worker, [js_compiler]},
             {device_compiler,
              {device_compiler, start_link, []},
              permanent, 5000, worker, [device_compiler]},
             {device_manager,
              {device_manager, start_link, []},
              permanent, 5000, worker, [device_manager]},
             {sketch_manager,
              {sketch_manager, start_link, []},
              permanent, 5000, worker, [sketch_manager]}
            ],
    {ok, {{one_for_one, 2, 7}, Procs}}.
