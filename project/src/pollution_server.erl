%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. kwi 2023 13:52
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Paweł").

%% API
-export([start/0, stop/0, init/0, add_station/2, add_value/4, remove_value/3,
  get_one_value/3, get_station_mean/2, get_daily_mean/2, get_location_value/2]).
-import(pollution, []).

start() ->
  register (pollution_server, spawn (?MODULE, init, [])).

init() ->
  Monitor = pollution:create_monitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    {request, Pid, add_station, [Name, Coordinates]} ->
      NewMonitor = pollution:add_station(Name, Coordinates, Monitor),
      case NewMonitor of
        {error, Msg} -> Pid ! {reply, error, Msg};
        _ -> Pid ! {reply, ok},
          loop(NewMonitor)
      end;

    {request, Pid, add_value, [StationInfo, Date, MeasurementType, Value]} ->
      NewMonitor = pollution:add_value(StationInfo, Date, MeasurementType, Value, Monitor),
      case NewMonitor of
        {error, Msg} -> Pid ! {reply, error, Msg};
        _ -> Pid ! {reply, ok},
          loop(NewMonitor)
      end;

    {request, Pid, remove_value, [StationInfo, Date, MeasurementType]} ->
      NewMonitor = pollution:remove_value(StationInfo, Date, MeasurementType, Monitor),
      case NewMonitor of
        {error, Msg} -> Pid ! {reply, error, Msg};
        _ -> Pid ! {reply, ok},
          loop(NewMonitor)
      end;

    {request, Pid, get_one_value, [StationInfo, Date, MeasurementType]} ->
      Value = pollution:get_one_value(StationInfo, Date, MeasurementType, Monitor),
      case Value of
        {error, Msg} -> Pid ! {reply, error, Msg};
        _ -> Pid ! {reply, Value},
          loop(Monitor)
      end;

    {request, Pid, get_station_mean, [StationInfo, MeasurementType]} ->
      StationMean = pollution:get_station_mean(StationInfo, MeasurementType, Monitor),
      case StationMean of
        {error, Msg} -> Pid ! {reply, error, Msg};
        _ -> Pid ! {reply, StationMean},
          loop(Monitor)
      end;

    {request, Pid, get_daily_mean, [MeasurementType, Day]} ->
      DailyMean = pollution:get_daily_mean(MeasurementType, Day, Monitor),
      case DailyMean of
        {error, Msg} -> Pid ! {reply, error, Msg};
        _ -> Pid ! {reply, DailyMean},
          loop(Monitor)
      end;

    {request, Pid, get_location_value, [Coordinates, Date]} ->
      Value = pollution:get_location_value(Coordinates, Date, Monitor),
      case Value of
        {error, Msg} -> Pid ! {reply, error, Msg};
        _ -> Pid ! {reply, Value},
          loop(Monitor)
      end;

    {request, Pid, stop, []} ->
      Pid ! {reply, ok}
end.

call(Function, Args) ->
  pollution_server ! {request, self(), Function, Args},
  receive
    {reply, error, Msg} -> {error, Msg};
    {reply, Reply} -> Reply
  end.

add_station(Name, Coordinates) -> call(add_station, [Name, Coordinates]).
add_value(StationInfo, Date, MeasurementType, Value) -> call(add_value, [StationInfo, Date, MeasurementType, Value]).
remove_value(StationInfo, Date, MeasurementType) -> call(remove_value, [StationInfo, Date, MeasurementType]).
get_one_value(StationInfo, Date, MeasurementType) -> call(get_one_value, [StationInfo, Date, MeasurementType]).
get_station_mean(StationInfo, MeasurementType) -> call(get_station_mean, [StationInfo, MeasurementType]).
get_daily_mean(MeasurementType, Day) -> call(get_daily_mean, [MeasurementType, Day]).
get_location_value(Coordinates, Date) -> call(get_location_value, [Coordinates, Date]).
stop() -> call(stop, []).
