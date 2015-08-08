%% ============================================================================
%%  Database server for record definitions.
%% ============================================================================

-module(recdump_def).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([insert/1, delete/1, lookup/1, to_list/0, clean/0, from_module/1]).
-export([insert_mod/1, delete_mod/1, lookup_mod/1, to_list_mod/0, ensure_loaded/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2,handle_info/2, code_change/3]).

-include("recdump.hrl").
-record(loop_data, {}).

%% ----------------------------------------------------------------------------
%%  O & M functions
%% ----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% ----------------------------------------------------------------------------
%%  API functions
%% ----------------------------------------------------------------------------

from_module(List) when is_list(List) ->
   ModNames = [ from_module(Item) || Item <- List],
   lists:flatten(ModNames);

from_module(Mod) when is_atom(Mod) ->
    case recdump_make:mod_defs(Mod) of
        {ok, Defs, _SrcFile} when is_list(Defs) -> 
            true = insert (Defs),
            true = insert_mod (Mod),
            [D#rec_def.name || D <- Defs];
        {error, Reason} -> {error, Reason}
    end.

ensure_loaded ([Mod | Tail]) ->
    case ensure_loaded (Mod) of
        ok -> ensure_loaded (Tail);
        {error, Reason} -> {error, Reason}
    end;
ensure_loaded ([]) -> ok;

ensure_loaded (Mod) when is_atom(Mod) ->
    case lookup_mod (Mod) of
        {ok, _} -> ok;
        {error, instance} -> 
            case from_module (Mod) of
                L when is_list(L) -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

insert(RecDef) ->
    gen_server:call(?MODULE, {insert, RecDef}).

delete(RecName) ->
    gen_server:call(?MODULE, {delete, RecName}).

lookup(RecName) ->
    recdump_def_db:lookup(RecName).

to_list() ->
    recdump_def_db:to_list().

insert_mod(ModName) ->
    gen_server:call(?MODULE, {insert_mod, ModName}).

delete_mod(ModName) ->
    gen_server:call(?MODULE, {delete_mod, ModName}).

lookup_mod(RecName) ->
    recdump_def_db:lookup_mod(RecName).

to_list_mod() ->
    recdump_def_db:to_list_mod().

clean() ->
    gen_server:call(?MODULE, clean).

%% ----------------------------------------------------------------------------
%%  gen_server callback functions
%% ----------------------------------------------------------------------------
init ([]) ->
    recdump_def_db:create_table(),
    LoopData = #loop_data{},
    {ok, LoopData}.

handle_call ({insert, RecDef}, _From, LoopData) ->
    Reply = recdump_def_db:insert(RecDef),
    {reply, Reply, LoopData};
handle_call ({delete, RecName}, _From, LoopData) ->
    Reply = recdump_def_db:delete(RecName),
    {reply, Reply, LoopData};
handle_call ({insert_mod, ModName}, _From, LoopData) ->
    Reply = recdump_def_db:insert_mod(ModName),
    {reply, Reply, LoopData};
handle_call ({delete_mod, ModName}, _From, LoopData) ->
    Reply = recdump_def_db:delete_mod(ModName),
    {reply, Reply, LoopData};
handle_call (clean, _From, LoopData) ->
    Reply = recdump_def_db:clean(),
    {reply, Reply, LoopData}.


handle_info (_Msg, LoopData) ->
    io:format("~p: unexpected message ~p~n", [?MODULE, _Msg]),
    {noreply, LoopData}.

handle_cast (stop, LoopData) ->
    {stop, normal, LoopData}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate (_Reason, _LoopData) ->
    recdump_def_db:close(),
    ok.
