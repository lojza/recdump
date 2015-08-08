%% ============================================================================
%%  Database operations for the recdump ETS table
%% ============================================================================

-module(recdump_def_db).

-export([create_table/0, close/0]).
-export([insert/1, delete/1, lookup/1, to_list/0, clean/0]).
-export([insert_mod/1, delete_mod/1, lookup_mod/1, to_list_mod/0]).

-include("recdump.hrl").

-define(TAB_DEFS, recdump_rec_defs).
-define(TAB_MODS, recdump_rec_mods).

-record (module, {
   name
}).

create_table() ->
    ets:new(?TAB_DEFS, [set, named_table , {keypos, #rec_def.name}]),
    ets:new(?TAB_MODS, [set, named_table , {keypos, #module.name}]).

close() ->
    ets:delete(?TAB_DEFS),
    ets:delete(?TAB_MODS).

insert(RecDef) ->
    ets:insert(?TAB_DEFS, RecDef).

insert_mod(ModName) ->
    ets:insert(?TAB_MODS, #module{name = ModName}).

delete (Name) ->
    ets:delete(?TAB_DEFS, Name).

delete_mod (ModName) ->
    ets:delete(?TAB_MODS, ModName).


lookup(Name) ->
    case ets:lookup (?TAB_DEFS, Name) of
        [RecDef] -> {ok, RecDef};
        [] -> {error, instance}
    end.

lookup_mod(ModName) ->
    case ets:lookup (?TAB_MODS, ModName) of
        [#module{name = N}] -> {ok, N};
        [] -> {error, instance}
    end.

to_list() ->
    ets:tab2list (?TAB_DEFS).

to_list_mod() ->
    [ N || #module{name = N} <- ets:tab2list (?TAB_MODS)].

clean() ->
    ets:delete_all_objects (?TAB_DEFS),
    ets:delete_all_objects (?TAB_MODS).
