-module(js_compiler).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([compile_js/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

compile_js(CppCode) -> 
    gen_server:call(?MODULE, {compile_js, CppCode}).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call({compile_js, CppCode}, _From, State) ->
    JsCompileDir = filename:join(code:priv_dir(treebuilder), "js_compile"),
    Wrapper = filename:join(JsCompileDir, "js_wrap.cpp"),
    
    InFile = lib:nonl(os:cmd("mktemp /tmp/XXXXXX.cpp")),
    OutFile = lib:nonl(os:cmd("mktemp /tmp/XXXXXX.js")),
    
    ok = file:write_file(InFile, CppCode),
    
    Cmd = ["/usr/lib/emscripten/em++",
           "-Wall", "-Werror", "-fcolor-diagnostics",
           "-s", "SIDE_MODULE=1",
           "-I", JsCompileDir,
           "-o", OutFile,
           Wrapper,
           InFile
          ],
    
    {Status, Props} = exec:run(Cmd, [sync, stdout, stderr]),
    
    Errors = case proplists:get_value(stderr, Props, null) of
                 null -> null;
                 ErrorParts -> erlang:iolist_to_binary(ErrorParts)
             end,
    JsCode = case Status of
                 ok ->
                     {ok, JsCode1} = file:read_file(OutFile),
                     JsCode1;
                 error ->
                     null
             end,
    
    file:delete(InFile), file:delete(OutFile),
    
    {reply, {JsCode, Errors}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
