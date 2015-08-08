%% ============================================================================
%%  Take the structure and save it to the file and load the content of the
%%  file to back the structure.
%% ============================================================================

-module(recdump).

-export([start/0,stop/0]).
-export([save/3,load/2]).
-export([save/2,load/1]).


%% ----------------------------------------------------------------------------
%%  start / stop record definitions database
%% ----------------------------------------------------------------------------

start() ->
    application:start(recdump).

stop() ->
    application:stop(recdump).


%% ----------------------------------------------------------------------------
%%  save / load data to the file with transformation from record tuple 
%%  to proplis
%% ----------------------------------------------------------------------------

save(Record, OutFile, RecDefModule) ->
    case recdump_def:ensure_loaded (RecDefModule) of
        ok ->
            Data = recdump_codec:serilalize (Record),    
            save (Data, OutFile);
        {error, Reason} -> {error, {load_def_error, Reason}}
    end.

load(InFile, RecDefModule) ->
    case recdump_def:ensure_loaded (RecDefModule) of
        ok ->
            case load(InFile) of
                {ok, Data} -> {ok, recdump_codec:deserialize (Data)};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, {load_def_error, Reason}}
    end.


%% ----------------------------------------------------------------------------
%%  save / load data to the file as is (without transformation)
%% ----------------------------------------------------------------------------

save (Data, OutFile) ->
    file:write_file(OutFile, io_lib:format("~p.~n", [Data])).

load (InFile) ->
    case file:consult(InFile) of
        {ok, [Data]} -> {ok, Data};
        {ok, _} -> {error, invalid_format};
        {error, Reason} -> {error, Reason}
    end.
