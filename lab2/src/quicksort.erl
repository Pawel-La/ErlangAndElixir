%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2023 15:13
%%%-------------------------------------------------------------------
-module(quicksort).
-author("Paweł").

%% API
-export([qs/1, random_elems/3, compare_speeds/3, how_many_divided_by_3/1, sum/1]).

qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) );
qs([]) -> [].

less_than(List, Arg) -> [X || X <- List, X < Arg].
grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

random_elems(N, Min, Max)-> [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1,N)].

get_time({Time, _}) -> Time.
compare_speeds(List, Fun1, Fun2) -> io:format("Fun 1 time: ~wms ~nFun 2 time: ~wms ~n",
  [get_time(timer:tc(Fun1, [List])), get_time(timer:tc(Fun2, [List]))]).

how_many_divided_by_3(List) -> how_many_divided_by_3(List, 0).
how_many_divided_by_3([H|T], Acc) when H rem 3 == 0 -> how_many_divided_by_3(T, Acc + 1);
how_many_divided_by_3([_|T], Acc) -> how_many_divided_by_3(T, Acc);
how_many_divided_by_3([], Acc) -> Acc.

sum(List) -> sum(List, 0).
sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum) -> Sum.
