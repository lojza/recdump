%%
%%
%%

-module (recdump_make).
-export([codec/2, mod_defs/1]).

-include("recdump.hrl").

%-define (DEBUG (STR, OPTS), io:format(STR ++ "~n", OPTS)).
-define (DEBUG (STR, OPTS), ok).

codec(Mod, Opts) ->
    case mod_defs(Mod) of
        {ok, Defs, SourceFile} ->
            OutFile0 = case proplists:get_value(out_dir, Opts) of
                undefined -> filename:absname(filename:basename(SourceFile));
                Dir -> filename:absname(filename:basename(SourceFile), Dir)
            end,
            OutFile1 = filename:rootname(OutFile0) ++ ".recdef",

            % not save the fields count
            FileDefs = [ {N, F} || #rec_def{name = N, fields = F} <- Defs],
            case file:write_file(OutFile1, io_lib:format("% === this file is generated ===~n% record definitions for module: ~w~n~n~p.~n", [Mod, FileDefs])) of
                ok -> [ N || #rec_def{name = N} <- Defs];
                Error -> Error
            end;
        Error -> Error
    end.

mod_defs(Mod) ->
    case find_file (Mod) of
        {ok, BeamFile} ->
            ?DEBUG("beam file ~p", [BeamFile]),
            case find_source (BeamFile) of
                {ok, SourceFile} ->
                    ?DEBUG("source file ~p", [SourceFile]),
                    case parse_file (SourceFile) of
                        {ok, Defs} -> {ok, Defs, SourceFile};
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end.    

parse_file (File) ->
    Cwd = ".",
    Dir = filename:dirname(File),
    IncludePath = case filename:basename(Dir) of
        "src" -> [Cwd,Dir, filename:dirname(Dir) ++ "/include"]; 
        _ -> [Cwd,Dir]
    end,

    ?DEBUG("parse file: ~p, include: ~p", [File, IncludePath]),
    case epp:parse_file (File, IncludePath, []) of
         {ok, Forms} -> {ok, record_defs (Forms)};
         Error -> Error
    end.

record_defs ([F | Tail]) ->
    case form_to_rec_def (F) of
        #rec_def{} = R -> [ R | record_defs(Tail)];
        _ -> record_defs(Tail)
    end;
record_defs([]) -> [].

form_to_rec_def ({attribute, _, record, {RecName, Attrs}}) ->
    case attr_names (Attrs) of
        L when is_list(L) -> #rec_def{name = RecName, size = length(L), fields = L};
        _ -> false
    end;
form_to_rec_def (_) -> false.

attr_names ([Attr | Tail]) ->
    case Attr of
        {record_field, _,{atom, _, AttrName}, _} when is_atom (AttrName) -> [AttrName | attr_names(Tail)];
        {record_field, _,{atom, _, AttrName}} when is_atom (AttrName) -> [AttrName | attr_names(Tail)];
        _ -> attr_names (Tail)
    end;
attr_names ([]) -> [].

% find file internal functions
find_file(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
        BeamFile when is_list(BeamFile) ->
            {ok, BeamFile};
        preloaded ->
            {_M,_Bin,BeamFile} = code:get_object_code(Mod),
            {ok, BeamFile};
        _Else ->
            {error,nofile}
    end.

find_source (BeamFile) ->
    Src0 = filename:rootname(BeamFile) ++ ".erl",
    case is_file(Src0) of
        true  -> {ok, Src0};
        false -> 
            EbinDir = filename:dirname(BeamFile),
            Src = filename:join([filename:dirname(EbinDir), "src",
                                 filename:basename(Src0)]),
            case is_file(Src) of
                true -> {ok, Src};
                false -> {error, nofile}
            end
    end.

is_file(Name) ->
    filelib:is_file(Name) andalso (not filelib:is_dir(Name)).
