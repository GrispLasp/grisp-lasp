-module(client_sensor).
-author('Alex Carlier').

-export([init/0,loop/3]).

init() ->
  io:format("Starting a client for the sensor ~n"),
  io:format("Adding node: ~p to the set clients ~n",[node()]),
  Time = os:timestamp(),
  lasp:update({<<"clients">>,state_orset},{add,{node(),Time}},self()),
  io:format("Creating a temperature and pressure sensor"),
  my_sensor:creates(temp),
  my_sensor:creates(press),
  loop([],[],1000).

loop(TempList,PressList,N) when N > 0->
  {AnswerPress,Pressure} = my_sensor:read(press),
  if AnswerPress == read -> io:format("Read pressure value is ~p ~n",[Pressure]);
    true -> exit(sensor_not_created)
    end,
    NewPressList = lists:append(PressList,[Pressure]),
  {AnswerTemp,Temp} = my_sensor:read(temp),
  if AnswerTemp == read -> io:format("Read temp value is ~p ~n",[Temp]);
    true -> exit(sensor_not_created)
    end,
    NewTempList = lists:append(TempList,[Temp]),
    loop(NewTempList,NewPressList,N-1);

  loop(TempList,PressList,0) ->
    AverageTemp = average(TempList),
    AveragePress = average(PressList),
    {ok, SetTemp} = lasp:update({<<"temp">>,state_orset},{add,AverageTemp},self()),
    io:format("After modifications the temp set is: ~p ~n",[lasp:query({<<"temp">>,state_orset})]),
    {ok, SetPress} = lasp:update({<<"press">>,state_orset},{add,AveragePress},self()),
    io:format("After modifications the temp set is: ~p ~n",[lasp:query({<<"press">>,state_orset})]),
    grisp_led:color(2,blue),
    ok.

  %%====================================================================
  %% Internal functions
  %%====================================================================
  average(List) -> sum(List)/length(List).

  sum([H|T]) -> H + sum(T);
  sum([]) -> 0.
