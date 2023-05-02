%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mar 2023 15:51
%%%-------------------------------------------------------------------
-module(distances).
-author("Paweł").

%% API
-export([start/2, find_closest_locker/3, receiver/3]).

start(NumberOfPeople, NumberOfLockers) ->
  PeopleLocations = [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, NumberOfPeople)],
  LockerLocations = [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, NumberOfLockers)],
  get_find_min_distance_speed(PeopleLocations, LockerLocations),
  get_faster_find_min_distance_speed(PeopleLocations, LockerLocations).

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

find_min_distance(PeopleLocations, LockerLocations) ->
  PeopleWithDistances = [find_closest_locker(PersonLocation, LockerLocations) ||
    PersonLocation <- PeopleLocations],
  lists:min(PeopleWithDistances).

find_closest_locker(PersonLocation, LockerLocations) ->
  LockersWithDistances = [{dist(LockerLocation, PersonLocation), LockerLocation} ||
    LockerLocation <- LockerLocations],
  {Dist, LL} = lists:min(LockersWithDistances),
  {Dist, {PersonLocation, LL}}.

get_time_in_seconds({Time, _}) -> Time/1000000.
get_find_min_distance_speed(PeopleLocations, LockerLocations) ->
  io:format("Fun 1:~nresult: ~w, time: ~w s~n",
    [find_min_distance(PeopleLocations, LockerLocations),
      get_time_in_seconds(timer:tc(fun find_min_distance/2, [PeopleLocations, LockerLocations]))]).

get_faster_find_min_distance_speed(PeopleLocations, LockerLocations) ->
  io:format("Fun 2~nresult: ~w, time: ~w s",
    [init(PeopleLocations, LockerLocations),
    get_time_in_seconds(timer:tc(fun init/2, [PeopleLocations, LockerLocations]))]).

init(PeopleLocations, LockerLocations) ->
  Pid = spawn(?MODULE, receiver, [[], length(PeopleLocations), self()]),
  [spawn(?MODULE, find_closest_locker, [PersonLocation, LockerLocations, Pid]) ||
    PersonLocation <- PeopleLocations],
  receive
    Result -> Result
  end.

find_closest_locker(PersonLocation, LockerLocations, Pid) ->
  Pid ! find_closest_locker(PersonLocation, LockerLocations).

receiver(State, DistancesNotReceived, Pid) ->
  receive
    Result when DistancesNotReceived > 1 ->
      receiver(State ++ [Result], DistancesNotReceived - 1, Pid);
    Result when DistancesNotReceived == 1 ->
      Pid ! lists:min(State ++ [Result])
  end.
