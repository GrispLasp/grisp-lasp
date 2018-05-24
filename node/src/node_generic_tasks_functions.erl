-module(node_generic_tasks_functions).
-compile(export_all).

temp_sensor({Counter, Temps}, PeriodicTime) ->

  WaitFun = fun(State) ->
      receive
      after (PeriodicTime) ->
        io:format("State is ~p and periodicTime is ~p ===~n", [State, PeriodicTime]),
        temp_sensor(State, PeriodicTime)
      end
  end,

  Sum = fun F(List) ->
          SumFun = fun ([H|T]) -> H + F(T);
              ([]) -> 0
          end,
          SumFun(List)
        end,

  Average = fun(List) -> Sum(List)/length(List) end,

  SensorFun = fun() ->
    io:format("=== Counter is at ~p ===~n", [Counter]),
    io:format("=== Temp list : ~p ===~n",[Temps]),
    case Counter of
      5 ->
        io:format("=== Timer has ended, aggregating data and updating CRDT... === ~n"),
        AverageTemp = Average(Temps),
        io:format("=== Average temp in past hour is ~p ===~n", [AverageTemp]),
        {ok, TempsCRDT} = lasp:query({<<"temp">>, state_orset}),
        TempsList = sets:to_list(TempsCRDT),
        io:format("=== Temps CRDT : ~p ===~n", [TempsList]),
        OldCrdtData = [{Node, OldAvg, HourCounter} || {Node, OldAvg, HourCounter} <- TempsList, Node =:= node()],
        io:format("=== Old CRDT data is ~p ===~n",[OldCrdtData]),
        case length(OldCrdtData) of
          0 ->
            lasp:update({<<"temp">>,state_orset},{add,{node(), AverageTemp, 1}}, self());
          1 ->
            {Node, OldAvg, HourCounter} = hd(OldCrdtData),
            NewAverageTemp = ((OldAvg * HourCounter)/(HourCounter+1))+(AverageTemp*(1/(HourCounter+1))),
            io:format("=== New average temp : ~p ===~n",[NewAverageTemp]),
            lasp:update({<<"temp">>, state_orset}, {rmv, {Node, OldAvg, HourCounter}}, self()),
            lasp:update({<<"temp">>,state_orset},{add,{node(), NewAverageTemp, HourCounter+1}}, self())
        end,
        {0, []};
      _ ->
        {AnswerTemp,Temp} = node_sensor_server_worker:read(temp),
        TempList = case AnswerTemp of
          read ->  lists:append(Temps,[Temp]);
          sensor_not_created -> exit(sensor_not_created)
        end,
        {Counter+1, TempList}
    end
  end,
  WaitFun(SensorFun()).
