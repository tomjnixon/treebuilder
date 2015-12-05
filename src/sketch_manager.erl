-module(sketch_manager).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% API.
-export([start_link/0]).
-export([install/1]).
-export([get_sketch_state/1]).
-export([save/2]).
-export([enable/1]).
-export([disable/1]).
-export([recompile/0]).

%% XXX: remove
-export([enabled_sketches/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
        current_sketches=[],
        current_hex=null,
        tree_synced=false
}).

-record(treebuilder_sketch, {
          name,
          cpp_code,
          versions=[],
          state=error, % error, compiles, enabled
          errors=null,
          js_code=null
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_sketch_state(Name) ->
    gen_server:call(?MODULE, {get_sketch_state, Name}).
save(Name, CppCode) ->
    gen_server:call(?MODULE, {save, Name, CppCode}).

enable(Name) ->
    gen_server:call(?MODULE, {enable, Name}).

disable(Name) ->
    gen_server:call(?MODULE, {disable, Name}).

recompile() ->
    gen_server:call(?MODULE, recompile).


install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(treebuilder_sketch,
                        [{attributes, record_info(fields, treebuilder_sketch)},
                         {index, [#treebuilder_sketch.state]},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

%% gen_server.

init([]) ->
    State=updated_code(#state{}, true, []),
    {ok, State}.

handle_call({get_sketch_state, Name}, _From, State) ->
    F = fun() -> case mnesia:read(treebuilder_sketch, Name) of
                    [] -> null;
                    [Sketch] -> get_user_state(Sketch)
                 end
        end,
    {reply, mnesia:activity(transaction, F), State};

handle_call(recompile, _From, State) ->
    {reply, ok, updated_code(State, true, [])};

handle_call({save, Name, CppCode}, _From, State) ->
    OldSketch = get_sketch_by_name_default(Name),
    
    NewSketch = OldSketch#treebuilder_sketch{cpp_code=CppCode},
    
    {ResultSketch, NewState} = make_change(State, OldSketch, NewSketch),
    
    {reply, {ok, get_user_state(ResultSketch)}, NewState};

handle_call({enable, Name}, _From, State) ->
    OldSketch = get_sketch_by_name_default(Name),
    
    case OldSketch#treebuilder_sketch.state of
        error ->
            {reply, {error, "cannot enable a sketch with errors"}, State};
        _ ->
            NewSketch = OldSketch#treebuilder_sketch{state=enabled},
            
            {ResultSketch, NewState} = make_change(State, OldSketch, NewSketch),
            {reply, {ok, get_user_state(ResultSketch)}, NewState}
    end;

handle_call({disable, Name}, _From, State) ->
    OldSketch = get_sketch_by_name_default(Name),
    
    NewSketch = OldSketch#treebuilder_sketch{state=compiles},
    
    {ResultSketch, NewState} = make_change(State, OldSketch, NewSketch),
    {reply, {ok, get_user_state(ResultSketch)}, NewState};

handle_call(get_state, _From, State) ->
    {reply, State, State};
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

%% private
get_sketch_by_name_default(Name) ->
    case mnesia:activity(transaction,
                         fun () -> mnesia:read(treebuilder_sketch, Name)
                         end) of
        [] -> #treebuilder_sketch{name=Name, cpp_code=none, js_code=none};
        [Sketch] -> Sketch
    end.

enabled_sketches() ->
    Match = ets:fun2ms(fun(#treebuilder_sketch{name=Name, cpp_code=CppCode, state=enabled}) ->
                            {Name, CppCode}
                       end),
    F = fun() -> mnesia:select(treebuilder_sketch, Match) end,
    lists:keysort(1, mnesia:activity(transaction, F)).

enabled_with_new(Name, CppCode) ->
    lists:keymerge(1,
                [{Name, CppCode}],
                enabled_sketches()).

% translate a treebuilder_sketch to the user-visible state proplist
get_user_state(#treebuilder_sketch{
                  name=Name,
                  cpp_code=CppCode,
                  state=State,
                  errors=Errors,
                  js_code=JsCode}) ->
    [{name, Name},
     {cpp_code, CppCode},
     {state, State},
     {errors, Errors},
     {js_code, JsCode}].

try_sync_tree(State=#state{tree_synced=true}) -> State;
try_sync_tree(State=#state{tree_synced=false, current_hex=Hex}) ->
    MessageParts = [{server,sketch_manager}, {hex, crypto:hash(md5, Hex)}],
    try device_manager:load_hex(Hex) of
        ok ->
            error_logger:info_report([wrote_hex | MessageParts]),
            State#state{tree_synced=true}
    catch
        % handle noproc so we don't spew large binaries in the usual cases.
        exit:{noproc, _} ->
            error_logger:error_report([error_writing_hex, {error, noproc} | MessageParts]),
            State#state{tree_synced=false};
        Error ->
            error_logger:error_report([error_writing_hex, {error, Error} | MessageParts]),
            State#state{tree_synced=false}
    end.

compile_v3(Sketches, ForceRecompile, State) ->
    compile_v3(Sketches, State, ForceRecompile, []).
compile_v3(Sketches, State, ForceRecompile, PreviousCompiles) ->
    % Sketches sorted by code
    SortedSketches = lists:keysort(2, Sketches),
    
    % Append the result in the current state if it's been set.
    % [{[{Name, Cpp}], Hex}]
    PreviousCompilesWState = case {ForceRecompile, State} of
                                 {true, _} -> [];
                                 {_, #state{current_hex=null}} -> PreviousCompiles;
                                 {_, #state{current_sketches=CurrentSketches,
                                        current_hex=CurrentHex}} ->
                                    [{CurrentSketches, CurrentHex} | PreviousCompiles]
                             end,
    % Extract just the code for each
    % [{Cpp], Hex}]
    PreviousCompilesFromCpp = [{element(2, lists:unzip(PreviousSketches)), PreviousHex}
                               || {PreviousSketches, PreviousHex} <- PreviousCompilesWState],
    
    {_Names, CppCodes} = lists:unzip(SortedSketches),
    case lists:keyfind(CppCodes, 1, PreviousCompilesFromCpp) of
        false ->
            case device_compiler:compile_for_device(CppCodes) of
                {ok, Hex, _Output} -> {ok, SortedSketches, Hex};
                {error, _Hex, Output} -> {error, Output}
            end;
        {_FoundCppCodes, FoundHex} -> {ok, SortedSketches, FoundHex}
    end.

try_compile_v3(State, Name, CppCode) ->
    Sketches = enabled_with_new(Name, CppCode),
    compile_v3(Sketches, State, false).

make_change(State, OldSketch, NewSketch) ->
    #treebuilder_sketch{name=OldName, cpp_code=OldCode, state=OldState} = OldSketch,
    #treebuilder_sketch{name=NewName, cpp_code=NewCode, state=NewState} = NewSketch,
    
    ForceRecompile=true,
    
    CompileResults = case ForceRecompile or (OldCode =/= NewCode) of
                         false ->
                             case OldState of
                                 error ->
                                     {ErrType, ErrString} = OldSketch#treebuilder_sketch.errors,
                                     {error, ErrType, ErrString};
                                 _ -> {ok, OldSketch#treebuilder_sketch.js_code, []}
                             end;
                         true ->
                             case js_compiler:compile_js(NewCode) of
                                 {null, JsErrors} ->
                                     {error, js_error, JsErrors};
                                 {JsCode, _JsErrors} ->
                                     case try_compile_v3(State, OldName, NewCode) of
                                         {ok, NewSketches, NewHex} ->
                                             {ok, JsCode, [{NewSketches, NewHex}]};
                                         {error, DeviceErrors} ->
                                             {error, device_error, DeviceErrors}
                                     end
                             end
                     end,
    
    NewWithResults = case CompileResults of
                         {error, ErrType1, ErrString1} ->
                             NewSketch#treebuilder_sketch{state=error,
                                                          errors={ErrType1, ErrString1},
                                                          js_code=null};
                         {ok, JsCodeRes, _} ->
                             NewSketch#treebuilder_sketch{errors=null,
                                                          js_code=JsCodeRes,
                                                          state=case NewState of
                                                                    error -> compiles;
                                                                    _ -> NewState
                                                                end}
                     end,
    
    F = fun () ->
                case OldName =:= NewName of
                    false -> mnesia:delete(treebuilder_sketch, OldName);
                    true -> do_nothing
                end,
                mnesia:write(NewWithResults)
        end,
    mnesia:activity(transaction, F),
    
    PreviousCompiles = case CompileResults of
                           {ok, _, Res} -> Res;
                           _ -> []
                       end,
    
    NewAppState = updated_code(State, false, PreviousCompiles),
    
    {NewWithResults, NewAppState}.

updated_code(State=#state{current_hex=CurrentHex, tree_synced=WasSynced}, ForceRecompile, PreviousCompiles) ->
    Sketches = enabled_sketches(),
    case compile_v3(Sketches, State, ForceRecompile, PreviousCompiles) of
        {error, Errors} ->
            error_logger:error_report(["failed to compile", Errors]),
            erlang:error(failed_to_compile);
        {ok, CompiledSketches, CompiledHex} ->
            Synced = WasSynced and (CompiledHex =:= CurrentHex),
            try_sync_tree(State#state{current_sketches=CompiledSketches,
                                      current_hex=CompiledHex,
                                      tree_synced=Synced})
    end.
