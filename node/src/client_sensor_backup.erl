-module(client_sensor_backup).
-author('Alex Carlier').

-export([init/0,loop/3]).

init() ->
  io:format("Starting a client for the sensor ~n"),
  io:format("Adding node: ~p to the set clients ~n",[node()]),
  Time = os:timestamp(),
  lasp:update({<<"clients">>,state_orset},{add,{node(),Time}},self()),
  io:format("Creating a temperature and pressure sensor~n"),
  node_sensor_worker:creates(temp),
  node_sensor_worker:creates(press),
  loop([],[],1000).

loop(TempList,PressList,N) when N > 0->
  {AnswerPress,Pressure} = node_sensor_worker:read(press),
  NewPressList = case AnswerPress of
    read -> lists:append(PressList,[Pressure]);
    sensor_not_created -> exit(sensor_not_created)
  end,
  {AnswerTemp,Temp} = node_sensor_worker:read(temp),
  NewTempList = case AnswerTemp of
    read ->  lists:append(TempList,[Temp]);
    sensor_not_created -> exit(sensor_not_created)
  end,
  loop(NewTempList,NewPressList,N-1);

  loop(TempList,PressList,0) ->
    AverageTemp = average(TempList),
    AveragePress = average(PressList),
    {ok, SetTemp} = lasp:update({<<"temp">>,state_orset},{add,{node(), AverageTemp}},self()),
    io:format("After modifications the temp set is: ~p ~n",[lasp:query({<<"temp">>,state_orset})]),
    {ok, SetPress} = lasp:update({<<"press">>,state_orset},{add,{node(), AveragePress}},self()),
    io:format("After modifications the temp set is: ~p ~n",[lasp:query({<<"press">>,state_orset})]),
    grisp_led:color(2,blue),
    ok.

  %%====================================================================
  %% Internal functions
  %%====================================================================
  average(List) -> sum(List)/length(List).

  sum([H|T]) -> H + sum(T);
  sum([]) -> 0.
