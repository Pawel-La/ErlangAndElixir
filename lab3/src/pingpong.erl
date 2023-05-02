%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mar 2023 15:19
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Paweł").

%% API
-export([start/0, play/1, stop/0, ping/1, pong/1, reset/0]).

start() ->
  register(ping, spawn(?MODULE, ping, [0])),
  register(pong, spawn(?MODULE, pong, [0])).

play(N) -> ping ! N.

ping(Sum) ->
  receive
    stop -> ok;
    reset -> pingpong:ping(0);
    0 -> pingpong:ping(Sum);
    N ->
      NewSum = Sum + N,
      io:format("Ping odbil: ~p, Suma odbitych: ~p~n", [N, NewSum]),
      timer:sleep(1000),
      pong ! (N - 1),
      pingpong:ping(NewSum)
  after
    20000 -> ok
  end.

pong(Sum) ->
  receive
    stop -> ok;
    reset -> pingpong:pong(0);
    0 -> pingpong:pong(Sum);
    N ->
      NewSum = Sum + N,
      io:format("Pong odbil: ~p, Suma odbitych: ~p~n", [N, NewSum]),
      timer:sleep(1000),
      ping ! (N - 1),
      pingpong:pong(NewSum)
  after
    20000 -> ok
  end.

reset() ->
  ping ! reset,
  pong ! reset.

stop() ->
  ping ! stop,
  pong ! stop.
