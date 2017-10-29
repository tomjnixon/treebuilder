-module(treebuilder_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [{"/js_compile", js_compile_handler, []},
                         {"/", cowboy_static, {priv_file, treebuilder, "static/index.html"}},
                         {"/assets/[...]", cowboy_static, {priv_dir, treebuilder, "static"}},
                         {"/sketches/[...]", sketches_handler, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    treebuilder_sup:start_link().

stop(_State) ->
    ok.
