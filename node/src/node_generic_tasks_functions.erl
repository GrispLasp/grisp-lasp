-module(node_generic_tasks_functions).
-compile(export_all).

nav_sensor(Comp, Register) ->
  % grisp:add_device(spi1, pmod_nav),
  % logger:log(info, "Value = ~p ~n", pmod_nav:read(alt, [press_out])),
  logger:log(info, "Value = ~p ~n", pmod_nav:read(Comp, [Register])).

meteorological_statistics(SampleCount, SampleInterval) ->
    % Must check if module is available
    {pmod_nav, Pid, _Ref} = node_util:get_nav(),
    % meteo = shell:rd(meteo, {press = [], temp = []}),
    % State = #{press => [], temp => [], time => []},
    State = maps:new(),
    State1 = maps:put(press, [], State),
    State2 = maps:put(temp, [], State1),
    State3 = maps:put(time, [], State2),
    FoldFun = fun
        (AccIn, Elem) when is_integer(Elem) andalso is_map(AccIn) ->
            timer:sleep(SampleInterval),
            T = node_stream_worker:maybe_get_time(),
            [Pr, Tmp] = gen_server:call(Pid, {read, alt, [press_out, temp_out], #{}}),
            #{press => maps:get(press, AccIn) ++ Pr,
            temp => maps:get(temp, AccIn) ++ Tmp,
            time => maps:get(time, AccIn) ++ [T]}
    end,

    M = lists:foldl(FoldFun, State3, lists:seq(1, SampleCount)),
    [Pressures, Temperatures, Epochs] = maps:values(M),
    Result = #{measures => lists:zip3(Epochs, Pressures, Temperatures),
        pmean => 'Elixir.Numerix.Statistics':mean(Pressures),
        pvar => 'Elixir.Numerix.Statistics':variance(Pressures),
        tmean => 'Elixir.Numerix.Statistics':mean(Temperatures),
        tvar => 'Elixir.Numerix.Statistics':variance(Temperatures),
        cov => 'Elixir.Numerix.Statistics':covariance(Pressures, Temperatures)},
    % L = lists:map(MapFun, lists:seq(1, SampleCount)),
    {ok, Id, _, _, _} = hd(node_util:declare_crdts([meteostats])),
    lasp:update(Id, {add, {node(), Result}}, self()).

% MapFun = fun
%     (Elem) when is_integer(Elem) ->
%         timer:sleep(SampleInterval),
%         T = node_stream_worker:maybe_get_time(),
%         Mes = gen_server:call(Pid, {read, alt, [press_out, temp_out], #{}}),
%         {T, Mes}
% end,
%#{press := Press, temp := Temp, time := Time}
% L = lists:map(MapFun, lists:seq(1, SampleCount)),

% Pressures = maps:get(press, M),
% Temperatures = maps:get(temp, M),
% Epochs = maps:get(time, M),
% [ X || X <- maps:values(M)],
    % ReceiverFun = fun (State#meteo{press = PList}) ->
    %     receive
    %
    %     after
    %         SampleInterval ->
    %             P = gen_server:call(Pid, {read, alt, [press_out], #{}}),
    %             NewState = #meteo{press = PList ++ P, temp = []},
    %             self() ! NewState
    %     end.
    %
    % ok.
        % ReceiverFun = fun () ->
        %     receive
        %         Data when is_list(Data) ->
        %             disconnect(),
        %             idle();
        %         {connect, B} ->
        %             B ! {busy, self()},
        %             wait_for_onhook()
        %     end,
        %
        %
        % PFun = fun
        %     (SampleInterval) ->
        %         [PressOut] = erlang:send_after(SampleInterval, Pid, {read, alt, [press_out], []})
        % end
        % wait_for_onhook() ->
        %     receive
        %         Data when is_list(Data) ->
        %             disconnect(),
        %             idle();
        %         {connect, B} ->
        %             B ! {busy, self()},
        %             wait_for_onhook()
        %     end.



temp_sensor({Counter, Temps}, PeriodicTime) ->

  WaitFun = fun(State) ->
      receive
      after (PeriodicTime) ->
        logger:log(info, "State is ~p and periodicTime is ~p ===~n", [State, PeriodicTime]),
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
    logger:log(info, "=== Counter is at ~p ===~n", [Counter]),
    % logger:log(info, "=== Temp list : ~p ===~n",[Temps]),
    logger:log(info, "=== Temp list : ~p ===~n",[Temps]),
    case Counter of
      5 ->
        logger:log(info, "=== Timer has ended, aggregating data and updating CRDT... === ~n"),
        AverageTemp = Average(Temps),
        logger:log(info, "=== Average temp in past hour is ~p ===~n", [AverageTemp]),
        {ok, TempsCRDT} = lasp:query({<<"temp">>, state_orset}),
        TempsList = sets:to_list(TempsCRDT),
        % logger:log(info, "=== Temps CRDT : ~p ===~n", [TempsList]),
        logger:log(info, "=== Temps CRDT : ~p ===~n", [TempsList]),
        OldCrdtData = [{Node, OldAvg, HourCounter, HourAvg, HourData} || {Node, OldAvg, HourCounter, HourAvg, HourData} <- TempsList, Node =:= node()],
        % logger:log(info, "=== Old CRDT data is ~p ===~n",[OldCrdtData]),
        logger:log(info, "=== Old CRDT data is ~p ===~n",[OldCrdtData]),
        case length(OldCrdtData) of
          0 ->
            lasp:update({<<"temp">>,state_orset},{add,{node(), AverageTemp, 1, [AverageTemp], [AverageTemp]}}, self());
          1 ->
            {Node, OldAvg, HourCounter, HourAvg, HourData} = hd(OldCrdtData),
            NewAverageTemp = ((OldAvg * HourCounter)/(HourCounter+1))+(AverageTemp*(1/(HourCounter+1))),
            logger:log(info, "=== New average temp : ~p ===~n",[NewAverageTemp]),
            lasp:update({<<"temp">>, state_orset}, {rmv, {Node, OldAvg, HourCounter, HourAvg, HourData}}, self()),
            lasp:update({<<"temp">>,state_orset},{add,{node(), NewAverageTemp, HourCounter+1, HourAvg ++ [NewAverageTemp], HourData ++ [AverageTemp]}}, self())
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


sonar_sensor(Mode, NodeTarget) ->
  SonarSensor = fun A() ->
    receive
      true ->
        {sonar_listener, NodeTarget} ! trigger,
        % {sonar_listener, node@my_grisp_board_2} ! {fuck}.
        spawn(fun () ->
          case Mode of
            in ->
              lasp:update({"<<enters>>", state_gcounter}, increment, self());
            out ->
              lasp:update({"<<exits>>", state_gcounter}, increment, self())
          end
        end),
        grisp_led:color(1,blue),
        grisp_led:color(2,blue),
      %  timer:sleep(500),
      %  grisp_led:color(1,green),
      %  grisp_led:color(2,green),
        A()
    end
  end,


  SonarListener = fun B() ->
    receive
      Msg ->
      %  logger:log(info, "=== received ~p ===~n", [Mode]),
        grisp_led:color(1,red),
        grisp_led:color(2,red),
        PidSonar = whereis(pmod_maxsonar),
        erlang:suspend_process(PidSonar),
        logger:log(notice, "suspending_process~n"),
        timer:sleep(750),
        erlang:resume_process(PidSonar),
        B()
    end
  end,

  PidSensor = spawn(SonarSensor),
  register(sonar_sensor, PidSensor),
  PidListener = spawn(SonarListener),
  register(sonar_listener, PidListener).
