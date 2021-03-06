-module(device_compiler).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([compile_for_device/1]).

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

compile_for_device(CppCodes) ->
    gen_server:call(?MODULE, {compile_for_device, CppCodes}, 30000).

%% gen_server.

init([]) ->
    {ok, #state{}}.

ensure_file_contents(FullPath, Contents) ->
    ShouldWrite = case file:read_file(FullPath) of
                      {error, _} -> true;
                      {ok, Contents} -> false;
                      {ok, _} -> true
                  end,

    case ShouldWrite of
        true -> file:write_file(FullPath, Contents);
        false -> ok
    end.

handle_call({compile_for_device, CppCodes}, _From, State) ->
    {ok, TemplateDir} = application:get_env(treebuilder, template_dir),
    SrcPath = filename:join(TemplateDir, "src"),
    SketchesPath = filename:join(TemplateDir, "sketches"),
    {ok, HexPathRel} = application:get_env(treebuilder, hex_path),
    HexPath = filename:join(TemplateDir, HexPathRel),
    
    CompileDir = filename:join(code:priv_dir(treebuilder), "compile"),

    {ok, PIOConfName} = application:get_env(treebuilder, pio_config),
    PIOConfSource = filename:join([code:priv_dir(treebuilder), "pio_config", PIOConfName]),

    % Clean out the old sketches and source
    exec:run([os:find_executable("rm"), "-rf", SrcPath, SketchesPath], [sync]),
    exec:run([os:find_executable("mkdir"), "-p", SketchesPath], [sync]),
    
    % put in the pio config
    PIOConfDest = filename:join(TemplateDir, "platformio.ini"),
    {ok, PIOConfContents} = file:read_file(PIOConfSource),
    ok = ensure_file_contents(PIOConfDest, PIOConfContents),

    % Put the correct code in the src dir.
    exec:run([os:find_executable("cp"), "-r", CompileDir, SrcPath], [sync]),
    
    Names = lists:map(fun erlang:integer_to_list/1,
                      lists:seq(0, length(CppCodes) - 1)),
    
    % write out the sketches
    lists:foreach(fun ({Name, CppCode}) ->
                        ok = file:write_file(filename:join(SketchesPath, [Name, ".cpp"]),
                                             CppCode)
                  end,
                  lists:zip(Names, CppCodes)),
    
    % write out the wrapper
    {ok, WrapSketchesCode} = list_modules_dtl:render([{names, Names}]),
    ok = file:write_file(filename:join(SrcPath, "wrap_sketches.cpp"),
                         WrapSketchesCode),
    
    % build it
    {Status, Props} = exec:run([os:find_executable("pio"), "run", "-v",
                                "-d", TemplateDir, "-t", HexPathRel],
                               [{stderr, stdout}, stdout, sync]),
    
    % get output
    Output = case proplists:get_value(stdout, Props, null) of
                 null -> null;
                 StdoutParts -> erlang:iolist_to_binary(StdoutParts)
             end,
    
    % read the hex file; assume this works if make completed
    Hex = case Status of
            ok ->
                  {ok, Hex1} = file:read_file(HexPath),
                Hex1;
            error ->
                null
          end,
    
    {reply, {Status, Hex, Output}, State};

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
