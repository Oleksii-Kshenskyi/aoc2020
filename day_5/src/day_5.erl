-module(day_5).

%% API exports
-export([main/1, line_to_base_2/1, char_to_bin/1]).

%%====================================================================
%% API functions
%%====================================================================

char_to_bin(70) -> 48;
char_to_bin(66) -> 49;
char_to_bin(82) -> 49;
char_to_bin(76) -> 48.

line_to_base_2(Line) ->
    String = binary_to_list(Line),
    Ints = lists:map(fun (Char) -> char_to_bin(Char) end, String),
    StringNum = list_to_bitstring(Ints),
    Row = list_to_integer(binary_to_list(string:slice(StringNum, 0, 7)), 2),
    Column = list_to_integer(binary_to_list(string:slice(StringNum, 7, 3)), 2),
    Row * 8 + Column.

file_to_base_2(FileList) ->
    SortedIDs = lists:sort(lists:map(fun (Line) -> line_to_base_2(Line) end, FileList)),
    lists:nthtail(length(SortedIDs) - 1, SortedIDs).

file_to_list(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    binary:split(Binary, <<"\n">>, [global, trim]).

%% escript Entry point
main([Filename]) ->
    FileList = file_to_list(Filename),
    Base2 = file_to_base_2(FileList),
    io:format("Largest ID: ~p~n", [Base2]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
