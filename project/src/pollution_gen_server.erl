%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2023 20:26
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-author("Paweł").

-export([crash/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2,
  get_daily_mean/2, get_location_value/2, stop/0]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-import(pollution, []).

%% START %%
start_link() -> gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
init(_) -> {ok, pollution:create_monitor()}.

%% CLIENT -> SERVER INTERFACE %%
crash() ->
  gen_server:cast(?MODULE,crash).
add_station(Name, Coordinates) ->
  gen_server:call(?MODULE, {add_station, [Name, Coordinates]}).
add_value(StationInfo, Date, MeasurementType, Value) ->
  gen_server:call(?MODULE, {add_value, [StationInfo, Date, MeasurementType, Value]}).
remove_value(StationInfo, Date, MeasurementType) ->
  gen_server:call(?MODULE, {remove_value, [StationInfo, Date, MeasurementType]}).
get_one_value(StationInfo, Date, MeasurementType) ->
  gen_server:call(?MODULE, {get_one_value, [StationInfo, Date, MeasurementType]}).
get_station_mean(StationInfo, MeasurementType) ->
  gen_server:call(?MODULE, {get_station_mean, [StationInfo, MeasurementType]}).
get_daily_mean(MeasurementType, Day) ->
  gen_server:call(?MODULE, {get_daily_mean, [MeasurementType, Day]}).
get_location_value(Coordinates, Date) ->
  gen_server:call(?MODULE, {get_location_value, [Coordinates, Date]}).
stop() ->
  gen_server:cast(?MODULE, stop).

%% MESSAGES SERVICE %%
handle_call({add_station, [Name, Coordinates]}, _From, Monitor) ->
  NewMonitor = pollution:add_station(Name, Coordinates, Monitor),
  case NewMonitor of
    {error, Msg} -> {stop, Msg, NewMonitor};
    _            -> {reply, ok, NewMonitor}
  end;
handle_call({add_value, [StationInfo, Date, MeasurementType, Value]}, _From, Monitor) ->
  NewMonitor = pollution:add_value(StationInfo, Date, MeasurementType, Value, Monitor),
  case NewMonitor of
    {error, Msg} -> {stop, Msg, NewMonitor};
    _            -> {reply, ok, NewMonitor}
  end;
handle_call({remove_value, [StationInfo, Date, MeasurementType]}, _From, Monitor) ->
  NewMonitor = pollution:remove_value(StationInfo, Date, MeasurementType, Monitor),
  case NewMonitor of
    {error, Msg} -> {stop, Msg, NewMonitor};
    _ -> {reply, ok, NewMonitor}
  end;
handle_call({get_one_value, [StationInfo, Date, MeasurementType]}, _From, Monitor) ->
  Value = pollution:get_one_value(StationInfo, Date, MeasurementType, Monitor),
  case Value of
    {error, Msg} -> {stop, Msg, Monitor};
    _ -> {reply, Value, Monitor}
  end;
handle_call({get_station_mean, [StationInfo, MeasurementType]}, _From, Monitor) ->
  StationMean = pollution:get_station_mean(StationInfo, MeasurementType, Monitor),
  case StationMean of
    {error, Msg} -> {stop, Msg, Monitor};
    _ -> {reply, StationMean, Monitor}
  end;
handle_call({get_daily_mean, [MeasurementType, Day]}, _From, Monitor) ->
  DailyMean = pollution:get_daily_mean(MeasurementType, Day, Monitor),
  case DailyMean of
    {error, Msg} -> {stop, Msg, Monitor};
    _ -> {reply, DailyMean, Monitor}
  end;
handle_call({get_location_value, [Coordinates, Date]}, _From, Monitor) ->
  Value = pollution:get_location_value(Coordinates, Date, Monitor),
  case Value of
    {error, Msg} -> {stop, Msg, Monitor};
    _ -> {reply, Value, Monitor}
  end.

handle_cast(stop, _Monitor) -> ok;
handle_cast(crash, N) -> 1/0, {noreply, N}.
