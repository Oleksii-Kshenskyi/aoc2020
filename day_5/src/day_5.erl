-module(day_5).

%% API exports
-export([main/1, line_to_ID/1, char_to_bin/1]).

char_to_bin(70) -> 48;
char_to_bin(66) -> 49;
char_to_bin(82) -> 49;
char_to_bin(76) -> 48.

line_to_ID(Line) ->
    String = binary_to_list(Line),
    Ints = lists:map(fun (Char) -> char_to_bin(Char) end, String),
    StringNum = list_to_bitstring(Ints),
    Row = list_to_integer(binary_to_list(string:slice(StringNum, 0, 7)), 2),
    Column = list_to_integer(binary_to_list(string:slice(StringNum, 7, 3)), 2),
    Row * 8 + Column.

find_missing_in_list_of_IDs([]) -> -1;
find_missing_in_list_of_IDs([H,H2|_]) when H + 1 =/= H2 ->
    H + 1;
find_missing_in_list_of_IDs([One, Two]) when One + 1 =/= Two -> 
    One;
find_missing_in_list_of_IDs([_|T]) -> find_missing_in_list_of_IDs(T).

file_to_answers(FileList) ->
    SortedIDs = lists:sort(lists:map(fun (Line) -> line_to_ID(Line) end, FileList)),
    Largest_ID = lists:nth(1, lists:nthtail(length(SortedIDs) - 1, SortedIDs)),
    Missing_ID = find_missing_in_list_of_IDs(SortedIDs),
    {Largest_ID, Missing_ID}.

file_to_list(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    binary:split(Binary, <<"\n">>, [global, trim]).

%% escript Entry point
main([Filename]) ->
    FileList = file_to_list(Filename),
    {LargestID, MissingID} = file_to_answers(FileList),
    io:format("Largest ID: ~p~nMissing ID: ~p~n", [LargestID, MissingID]),
    erlang:halt(0).
