-module(day_13).

%% API exports
-export([main/1]).

solve_file(Filename) ->
    {ok, Contents} = file:read_file(Filename),
    io:format("=====~s:~n~s~n", [Filename, Contents]).

solve_all([]) -> ok;
solve_all([Name|Tail]) -> solve_file(Name), solve_all(Tail).

main(Args) ->
    solve_all(Args),
    erlang:halt(0).

