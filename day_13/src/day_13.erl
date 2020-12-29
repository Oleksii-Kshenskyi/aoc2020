-module(day_13).

%% API exports
-export([main/1]).

-record(bus_crap_1, { earliestT = 0, busIDs = [] } ).
-record(bus_crap_2, { busID = 0, offset = 0} ).

bus_crap_for_part_1_from_file(Contents) -> 
    LineByLine = binary:split(Contents, [<<"\n">>], [global, trim]),
    EarliestT = binary_to_integer(lists:nth(1, LineByLine)),
    BusIDs = lists:map(fun(Int) -> binary_to_integer(Int) end, lists:filter(fun(Str) -> Str =/= <<"x">> end, binary:split(lists:nth(2, LineByLine), [<<",">>], [global, trim]))),
    #bus_crap_1{earliestT = EarliestT, busIDs = BusIDs}.

gen_bus_crap_2(BC2Records, _, []) -> BC2Records;
gen_bus_crap_2(BC2Records, CurrentOffset, [IDH|IDT]) when IDH =/= <<"x">> ->
    gen_bus_crap_2(BC2Records ++ [#bus_crap_2 { busID = binary_to_integer(IDH), offset = CurrentOffset}], CurrentOffset + 1, IDT);
gen_bus_crap_2(BC2Records, CurrentOffset, [_|IDT]) ->
    gen_bus_crap_2(BC2Records, CurrentOffset + 1, IDT).
gen_bus_crap_2(RawListOfIDs) -> gen_bus_crap_2([], 0, RawListOfIDs).


bus_crap_for_part_2_from_file(Contents) -> 
    Unfiltered = binary:split(lists:nth(2, binary:split(Contents, <<"\n">>, [global, trim])), <<",">>, [global, trim]),
    gen_bus_crap_2(Unfiltered).



solve_part_1(EarliestT, CurrentT, _, [BIH]) when CurrentT rem BIH == 0 -> (CurrentT - EarliestT) * BIH;
solve_part_1(EarliestT, CurrentT, BusIDs, [_]) -> solve_part_1(EarliestT, CurrentT + 1, BusIDs, BusIDs);
solve_part_1(EarliestT, CurrentT, _, [BIH|_]) when CurrentT rem BIH == 0 -> (CurrentT - EarliestT) * BIH;
solve_part_1(EarliestT, CurrentT, BusIDs, [_|BIT]) -> solve_part_1(EarliestT, CurrentT, BusIDs, BIT).
solve_part_1(EarliestT, BusIDs) -> solve_part_1(EarliestT, EarliestT, BusIDs, BusIDs).


solve_part_2(Timestamp, Step, BusCrap) ->
    case is_timestamp_the_solution(Timestamp, BusCrap) of
        true -> Timestamp;
        false -> solve_part_2(Timestamp + Step, 1, BusCrap)
    end.
solve_part_2(Timestamp, BusCrap) -> solve_part_2(Timestamp, 1, BusCrap).



part1_solution_from_bus_crap_1(Crap) -> solve_part_1(Crap#bus_crap_1.earliestT, Crap#bus_crap_1.busIDs).

check_timestamp_or_false(Timestamp, ID, Offset, []) when (Timestamp + Offset) rem ID =/= 0 -> false;
check_timestamp_or_false(_, _, _, []) -> true;
check_timestamp_or_false(Timestamp, ID, Offset, _) when (Timestamp + Offset) rem ID =/= 0 -> false;
check_timestamp_or_false(Timestamp, _, _, CRAPTAIL) -> is_timestamp_the_solution(Timestamp, CRAPTAIL).
is_timestamp_the_solution(Timestamp, [CRAPHEAD|CRAPTAIL]) ->
    {ID, Offset} = {CRAPHEAD#bus_crap_2.busID, CRAPHEAD#bus_crap_2.offset},
    check_timestamp_or_false(Timestamp, ID, Offset, CRAPTAIL).



part2_solution_from_bus_crap_2(Crap) -> solve_part_2(1, Crap).





part_1(Contents) ->
    BusCrap = bus_crap_for_part_1_from_file(Contents),
    io:format("Part 1: Earliest bus ID * minutes to wait: ~w;~n", 
              [part1_solution_from_bus_crap_1(BusCrap)]).

part_2(Contents) ->
    BusCrap = bus_crap_for_part_2_from_file(Contents),
    io:format("Part 2: Earliest timestamp with subsequent departures: ~p;~n",
              [part2_solution_from_bus_crap_2(BusCrap)]).

solve_file(Filename) ->
    {ok, Contents} = file:read_file(Filename),
    io:format("=====~s:=====~n", [Filename]),
    part_1(Contents),
    part_2(Contents).

solve_all([]) -> ok;
solve_all([Name|Tail]) -> solve_file(Name), solve_all(Tail).

main(Args) -> solve_all(Args).

