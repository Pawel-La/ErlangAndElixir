%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2023 21:39
%%%-------------------------------------------------------------------
-module(pollution).
-author("Paweł").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4,
  get_one_value/4, get_station_mean/3, get_daily_mean/3, get_location_value/3]).

-record(station, {name, coordinates, measurements=#{}}).
-record(measurement, {date_and_hour, type, value}).

create_monitor() -> #{}.

add_station(Name, Coordinates, Monitor) ->
  StationFilter = fun({X, Y}, _) -> (X == Name) or (Y == Coordinates) end,
  SubMonitor = maps:filter(StationFilter, Monitor),
  case maps:size(SubMonitor) of
    0 -> Monitor#{{Name, Coordinates} => #station{coordinates = Coordinates, name = Name}};
    _ -> {error, "Station with given name/coordinates already exists"}
  end.

add_value(StationInfo, Date, MeasurementType, Value, Monitor) ->
  StationFilter = fun({X, Y}, _) -> (X == StationInfo) or (Y == StationInfo) end,
  SubMonitor = maps:filter(StationFilter, Monitor),
  case maps:size(SubMonitor) of
    0 -> {error, "Station with given name/coordinates doesn't exist"};
    1 ->
      [{StationName, Coordinates}] = maps:keys(SubMonitor),
      [Station] = maps:values(SubMonitor),
      Measurements = Station#station.measurements,
      MeasurementsFilter = fun({Date_, MeasurementType_}, _) ->
        (Date_ == Date) and (MeasurementType_ == MeasurementType) end,
      SubMeasurements = maps:filter(MeasurementsFilter, Measurements),
      case maps:size(SubMeasurements) of
        1 -> {error, "Measurement with given date and type already exists"};
        0 -> UpdatedMeasurements =
          Measurements#{{Date, MeasurementType} => #measurement{
            date_and_hour = Date,
            type = MeasurementType,
            value = Value}
          },
          UpdatedStation = Station#station{measurements = UpdatedMeasurements},
          Monitor#{{StationName, Coordinates} := UpdatedStation}
      end
  end.

remove_value(StationInfo, Date, MeasurementType, Monitor) ->
  StationFilter = fun({X, Y}, _) -> (X == StationInfo) or (Y == StationInfo) end,
  SubMonitor = maps:filter(StationFilter, Monitor),
  case maps:size(SubMonitor) of
    0 -> {error, "Station with given name/coordinates doesn't exist"};
    1 ->
      [{StationName, Coordinates}] = maps:keys(SubMonitor),
      [Station] = maps:values(SubMonitor),
      Measurements = Station#station.measurements,
      case maps:find({Date, MeasurementType}, Measurements) of
        error -> {error, "Measurement with given date and type doesn't exist"};
        {ok, _} -> UpdatedMeasurements = maps:remove({Date, MeasurementType}, Measurements),
          UpdatedStation = Station#station{measurements = UpdatedMeasurements},
          Monitor#{{StationName, Coordinates} := UpdatedStation}
      end
  end.

get_one_value(StationInfo, Date, MeasurementType, Monitor) ->
  StationFilter = fun({X, Y}, _) -> (X == StationInfo) or (Y == StationInfo) end,
  SubMonitor = maps:filter(StationFilter, Monitor),
  case maps:size(SubMonitor) of
    0 -> {error, "Station with given name/coordinates doesn't exist"};
    1 ->
      [Station] = maps:values(SubMonitor),
      Measurements = Station#station.measurements,
      case maps:find({Date, MeasurementType}, Measurements) of
        error -> {error, "Measurement with given date and type doesn't exist"};
        {ok, #measurement{value = Value}} -> Value
      end
  end.

get_station_mean(StationInfo, MeasurementType, Monitor) ->
  StationFilter = fun({X, Y}, _) -> (X == StationInfo) or (Y == StationInfo) end,
  SubMonitor = maps:filter(StationFilter, Monitor),
  case maps:size(SubMonitor) of
    0 -> {error, "Station with given name/coordinates doesn't exist"};
    1 ->
      [Station] = maps:values(SubMonitor),
      get_station_mean(maps:to_list(Station#station.measurements), MeasurementType, 0, 0)
  end.

get_station_mean([{_, #measurement{type = MeasurementType, value = Value}} | T],
    MeasurementType, Sum, NumberOfValues) ->
  get_station_mean(T, MeasurementType, Sum + Value, NumberOfValues + 1);
get_station_mean([_|T], MeasurementType, Sum, NumberOfValues) ->
  get_station_mean(T, MeasurementType, Sum, NumberOfValues);
get_station_mean([], _, _, 0) -> {error, "No measurements with given type in given station"};
get_station_mean([], _, Sum, NumberOfValues) -> Sum / NumberOfValues.

get_daily_mean(MeasurementType, Day, Monitor) ->
  get_daily_mean(MeasurementType, Day, maps:to_list(Monitor), 0, 0).

get_daily_mean(MeasurementType, Day, [{_, #station{measurements = Measurements}}|T], Sum, NumberOfValues) ->
  {X, Y} = get_daily_measurements_sum_and_number_of_values(MeasurementType, Day, maps:to_list(Measurements), 0, 0),
  get_daily_mean(MeasurementType, Day, T, Sum + X, NumberOfValues + Y);
get_daily_mean(_, _, [], _, 0) -> {error, "No measurements with given type in given station"};
get_daily_mean(_, _, [], Sum, NumberOfValues) -> Sum / NumberOfValues.

get_daily_measurements_sum_and_number_of_values(MeasurementType, Day,
    [{_, #measurement{type = MeasurementType, date_and_hour = {Day,_}, value = Value}} | T], Sum, NumberOfValues) ->
  get_daily_measurements_sum_and_number_of_values(MeasurementType, Day, T, Sum + Value, NumberOfValues + 1);
get_daily_measurements_sum_and_number_of_values(MeasurementType, Day, [_|T], Sum, NumberOfValues) ->
  get_daily_measurements_sum_and_number_of_values(MeasurementType, Day, T, Sum, NumberOfValues);
get_daily_measurements_sum_and_number_of_values(_, _, [], Sum, NumberOfValues) -> {Sum, NumberOfValues}.

get_location_value(Coordinates, Date, Monitor) ->
  StationsClosestToCoordinates = get_stations_closest_to_coordinates(Coordinates, maps:to_list(Monitor)),
  GetListOfClosestMeasurementsValues = get_list_of_closest_measurements_to_date(StationsClosestToCoordinates, Date),
  list_mean(GetListOfClosestMeasurementsValues).

get_stations_closest_to_coordinates(Coordinates, StationsList) ->
  StationsListSortedByDistances = sort_stations_by_distances_from_coordinates(Coordinates, StationsList),
  get_first_three_elements(StationsListSortedByDistances).

sort_stations_by_distances_from_coordinates(Coordinates, StationsList) ->
  Fun = fun(#station{coordinates = Coordinates1}, #station{coordinates = Coordinates2}) ->
    get_distance_between_coordinates(Coordinates, Coordinates1) <
      get_distance_between_coordinates(Coordinates, Coordinates2) end,
  lists:sort(Fun, StationsList).

get_distance_between_coordinates({N1, E1}, {N2, E2}) ->
  math:sqrt((N1 - N2) * (N1 - N2) + (E1 - E2) * (E1 - E2)).

get_first_three_elements([H1,H2,H3|_]) -> [H1,H2,H3];
get_first_three_elements(List) -> List.

get_list_of_closest_measurements_to_date(StationsClosestToCoordinates, Date) ->
  get_list_of_closest_measurements_to_date(StationsClosestToCoordinates, Date, []).
get_list_of_closest_measurements_to_date([Station|Tail], Date, Result) ->
  Value = get_value_of_closest_measurement_to_date_from_station(Station, Date),
  get_list_of_closest_measurements_to_date(Tail, Date, [Value|Result]);
get_list_of_closest_measurements_to_date([], _, Result) -> Result.

get_value_of_closest_measurement_to_date_from_station(#station{measurements = Measurements}, Date) ->
  Measurement = get_closest_measurement_to_date(maps:to_list(Measurements), Date),
  Measurement#measurement.value.

get_closest_measurement_to_date(Measurements, Date) ->
  Fun = fun(#measurement{date_and_hour = Date1}, #measurement{date_and_hour = Date2}) ->
    get_time_diff(Date, Date1) < get_time_diff(Date, Date2) end,
  [H|_] = lists:sort(Fun, Measurements),
  H.

get_time_diff(Data1, Data2) when Data1 < Data2 -> calendar:time_difference(Data1, Data2);
get_time_diff(Data1, Data2) -> calendar:time_difference(Data2, Data1).

list_mean(List) -> list_mean(List, 0, 0).
list_mean([H|T], Sum, NumberOfValues) -> list_mean(T, Sum+H, NumberOfValues+1);
list_mean([], _, 0) -> {error, "empty list"};
list_mean([], Sum, NumberOfValues) -> Sum/NumberOfValues.
