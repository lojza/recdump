%% ============================================================================
%%  Record dump serialization and deserialization
%% ============================================================================

-module(recdump_codec).

-export([serilalize/1, deserialize/1]).

-include("recdump.hrl").

%% ----------------------------------------------------------------------------
%%  API functions
%% ----------------------------------------------------------------------------

serilalize(Record) ->
    records_to_proplists (Record).

deserialize(PropList) -> 
    proplists_to_records (PropList).

%% ----------------------------------------------------------------------------
%%  internal functions
%% ----------------------------------------------------------------------------

records_to_proplists (Term) when is_tuple (Term) ->
    case check_record (Term) of
        false -> list_to_tuple(records_to_proplists(tuple_to_list (Term)));
        F -> 
            [Tag | Values] = tuple_to_list (Term), 
            DecValues = records_to_proplists(Values),
            {Tag, lists:zip(F, DecValues)}
    end;
records_to_proplists (List) when is_list (List) ->
    [ records_to_proplists (Item) || Item <- List];
records_to_proplists (Term) -> Term.

proplists_to_records ({Name, LisOfValues}) when is_atom(Name) and is_list(LisOfValues) ->
    Size = length(LisOfValues),
    case find_record_fields (Name, Size) of
        false -> {Name, proplists_to_records(LisOfValues)};
        FieldNames ->
            case check_proplist(LisOfValues, FieldNames) of
                false -> {Name, proplists_to_records(LisOfValues)};
                true -> 
                    % checked values in correct order
                    OrderdValues = [ proplists:get_value(FN, LisOfValues) || FN <- FieldNames],
                    % converted values
                    ConvertedValues = proplists_to_records (OrderdValues),
                    % prepend name and make a tuple
                    list_to_tuple ([Name | ConvertedValues])
            end
    end;
proplists_to_records (List) when is_list(List) ->
    [ proplists_to_records (Item) || Item <- List];
proplists_to_records (Term) -> Term.

% check if term seems to be a record and return its fields if we have its definition
check_record (Record) when is_tuple(Record) and (size(Record) > 0) and is_atom(element(1, Record)) ->
    Name = element(1, Record),
    Size = size(Record) - 1,
    find_record_fields (Name, Size);
check_record (_) -> false.

% find fields for record, or return false
find_record_fields (Name, Size) ->
    case recdump_def:lookup (Name) of
        {ok, #rec_def{size = S, fields = Fields}} when S == Size -> Fields;
        {ok, #rec_def{}} -> false;
        {error,instance} -> false
    end.

% check if proplist match to 
check_proplist ([Item | Tail], FieldNames) ->
    case Item of
        {Key, _Value} ->
            case in_list (FieldNames, Key) of
                true -> check_proplist (Tail, lists:delete(Key, FieldNames));
                false -> false
            end;
        _ -> false
    end;
check_proplist ([], []) -> true;
check_proplist ([], _MissingFields) -> false.
    
in_list ([Key | _T], Key) -> true;
in_list ([_H | T], Key) -> in_list (T, Key);
in_list ([], _Key) -> false.
