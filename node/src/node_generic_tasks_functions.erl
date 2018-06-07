-module(node_generic_tasks_functions).

-compile(export_all).

temp_sensor({Counter, Temps}, PeriodicTime) ->
    WaitFun = fun (State) ->
		      receive
			
			after PeriodicTime ->
				  io:format("State is ~p and periodicTime is ~p ===~n",
					    [State, PeriodicTime]),
				  temp_sensor(State, PeriodicTime)
		      end
	      end,
    Sum = fun F(List) ->
		  SumFun = fun ([H | T]) -> H + F(T);
			       ([]) -> 0
			   end,
		  SumFun(List)
	  end,
    Average = fun (List) -> Sum(List) / length(List) end,
    SensorFun = fun () ->
			io:format("=== Counter is at ~p ===~n", [Counter]),
			io:format("=== Temp list : ~p ===~n", [Temps]),
			case Counter of
			  5 ->
			      io:format("=== Timer has ended, aggregating data "
					"and updating CRDT... === ~n"),
			      AverageTemp = Average(Temps),
			      io:format("=== Average temp in past hour is ~p "
					"===~n",
					[AverageTemp]),
			      {ok, TempsCRDT} = lasp:query({<<"temp">>,
							    state_orset}),
			      TempsList = sets:to_list(TempsCRDT),
			      io:format("=== Temps CRDT : ~p ===~n",
					[TempsList]),
			      OldCrdtData = [{Node, OldAvg, HourCounter,
					      HourAvg, HourData}
					     || {Node, OldAvg, HourCounter,
						 HourAvg, HourData}
						    <- TempsList,
						Node =:= node()],
			      io:format("=== Old CRDT data is ~p ===~n",
					[OldCrdtData]),
			      case length(OldCrdtData) of
				0 ->
				    lasp:update({<<"temp">>, state_orset},
						{add,
						 {node(), AverageTemp, 1,
						  [AverageTemp],
						  [AverageTemp]}},
						self());
				1 ->
				    {Node, OldAvg, HourCounter, HourAvg,
				     HourData} =
					hd(OldCrdtData),
				    NewAverageTemp = OldAvg * HourCounter /
						       (HourCounter + 1)
						       +
						       AverageTemp *
							 (1 /
							    (HourCounter + 1)),
				    io:format("=== New average temp : ~p ===~n",
					      [NewAverageTemp]),
				    lasp:update({<<"temp">>, state_orset},
						{rmv,
						 {Node, OldAvg, HourCounter,
						  HourAvg, HourData}},
						self()),
				    lasp:update({<<"temp">>, state_orset},
						{add,
						 {node(), NewAverageTemp,
						  HourCounter + 1,
						  HourAvg ++ [NewAverageTemp],
						  HourData ++ [AverageTemp]}},
						self())
			      end,
			      {0, []};
			  _ ->
			      {AnswerTemp, Temp} =
				  node_sensor_server_worker:read(temp),
			      TempList = case AnswerTemp of
					   read -> Temps ++ [Temp];
					   sensor_not_created ->
					       exit(sensor_not_created)
					 end,
			      {Counter + 1, TempList}
			end
		end,
    WaitFun(SensorFun()).

sonar_sensor(Mode, NodeTarget) ->
    SonarSensor = fun A() ->
			  receive
			    true ->
				{sonar_listener, NodeTarget} ! trigger,
				% {sonar_listener, node@my_grisp_board_2} ! {fuck}.
				spawn(fun () ->
					      case Mode of
						in ->
						    lasp:update({"<<enters>>",
								 state_gcounter},
								increment,
								self());
						out ->
						    lasp:update({"<<exits>>",
								 state_gcounter},
								increment,
								self())
					      end
				      end),
				grisp_led:color(1, blue),
				grisp_led:color(2, blue),
				%  timer:sleep(500),
				%  grisp_led:color(1,green),
				%  grisp_led:color(2,green),
				A()
			  end
		  end,
    SonarListener = fun B() ->
			    receive
			      Msg ->
				  %  io:format("=== received ~p ===~n", [Mode]),
				  grisp_led:color(1, red),
				  grisp_led:color(2, red),
				  PidSonar = whereis(pmod_maxsonar),
				  erlang:suspend_process(PidSonar),
				  io:format("suspending_process~n"),
				  timer:sleep(750),
				  erlang:resume_process(PidSonar),
				  B()
			    end
		    end,
    PidSensor = spawn(SonarSensor),
    register(sonar_sensor, PidSensor),
    PidListener = spawn(SonarListener),
    register(sonar_listener, PidListener).
