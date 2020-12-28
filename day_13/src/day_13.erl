-module(day_13).

%% API exports
-export([main/1]).

-record(bus_crap, { earliestT = 0, busIDs = [] } ).

bus_crap_from_file(Contents) -> 
    LineByLine = binary:split(Contents, [<<"\n">>], [global, trim]),
    EarliestT = binary_to_integer(lists:nth(1, LineByLine)),
    BusIDs = lists:map(fun(Int) -> binary_to_integer(Int) end, lists:filter(fun(Str) -> Str =/= <<"x">> end, binary:split(lists:nth(2, LineByLine), [<<",">>], [global, trim]))),
    #bus_crap{earliestT = EarliestT, busIDs = BusIDs}.


solve_part_1(EarliestT, CurrentT, _, [BIH]) when CurrentT rem BIH == 0 -> (CurrentT - EarliestT) * BIH;
solve_part_1(EarliestT, CurrentT, BusIDs, [_]) -> solve_part_1(EarliestT, CurrentT + 1, BusIDs, BusIDs);
solve_part_1(EarliestT, CurrentT, _, [BIH|_]) when CurrentT rem BIH == 0 -> (CurrentT - EarliestT) * BIH;
solve_part_1(EarliestT, CurrentT, BusIDs, [_|BIT]) -> solve_part_1(EarliestT, CurrentT, BusIDs, BIT).
solve_part_1(EarliestT, BusIDs) -> solve_part_1(EarliestT, EarliestT, BusIDs, BusIDs).

part1_solution_from_bus_crap(Crap) -> solve_part_1(Crap#bus_crap.earliestT, Crap#bus_crap.busIDs).

part_1(Contents) ->
    io:format("Part 1: Earliest bus ID * minutes to wait: ~w;~n", [part1_solution_from_bus_crap(Contents)]).

solve_file(Filename) ->
    {ok, Contents} = file:read_file(Filename),
    BusCrap = bus_crap_from_file(Contents),
    io:format("=====~s:=====~n", [Filename]),
    part_1(BusCrap).

solve_all([]) -> ok;
solve_all([Name|Tail]) -> solve_file(Name), solve_all(Tail).

main(Args) ->
    solve_all(Args),
    erlang:halt(0).

