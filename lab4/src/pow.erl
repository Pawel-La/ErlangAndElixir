%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2023 18:21
%%%-------------------------------------------------------------------
-module(pow).
-behaviour(gen_server).
%% API
-export([start_link/0, step/0, read/0, close/0, crash/0, set/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE},?MODULE,2,[]).
init(N)        -> {ok,N}.

%% INTERFEJS KLIENT -> SERWER %%
step()        -> gen_server:cast(?MODULE,step).
crash()       -> gen_server:cast(?MODULE,crash).

set(NewValue) -> gen_server:call(?MODULE,{set, NewValue}).
read()        -> gen_server:call(?MODULE,read).
close()       -> gen_server:call(?MODULE,terminate).

%% OBSŁUGA WIADOMOŚCI %%
handle_cast(step, N) -> {noreply, N*N};
handle_cast(crash, N) -> no:exist(), {noreply, N}.
%%handle_cast({set, NewValue}, _) -> {noreply, NewValue}.

handle_call(read,_From, N)      -> {reply, N, N};
handle_call(terminate,_From,N) -> {stop, normal, ok, N};
handle_call({set, NewValue},_From, _) -> {reply, NewValue, NewValue}.

handle_info(Message, _From) ->
  io:format("Message received: ~w~n",[Message]),
  {noreply, _From}.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.
