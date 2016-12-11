-module(pinger_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hosts = application:get_env(pinger, ping_hosts, []),
    Delay = application:get_env(pinger, delay, 5000),

    Procs = [{pinger,
              {pinger, start_link, [Delay, Hosts]},
              permanent, 5000, worker, [pinger]}
            ],

    {ok, {{one_for_one, 1, 5}, Procs}}.
