-module(node_sensor_client_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, terminate/0]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {counter, temps, ranges}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

terminate() -> gen_server:call(?MODULE, {terminate}).

%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init([]) ->
  io:format("Starting a client for the sensor ~n"),
  % io:format("Adding node: ~p to the set clients ~n",[node()]),
  % Time = os:timestamp(),
  % lasp:update({<<"clients">>,state_orset},{add,{node(),Time}},self()),
  % io:format("Creating a MAXSonar sensor~n"),
  % node_sensor_server_worker:creates(pmod_maxsonar),
  io:format("Creating an ACL sensor~n"),
  node_sensor_server_worker:creates(pmod_acl2),
  timer:sleep(15000),
  io:format("Creating a temperature sensor~n"),
  node_sensor_server_worker:creates(temp),
  {ok, #state{counter=0, temps=[], ranges=[]}, 30000}.

handle_call(stop, _From, State) ->
{stop, normal, ok, State}.

handle_info(timeout, S = #state{counter=Counter, temps=Temps, ranges=Ranges}) ->
    io:format("=== Counter is at ~p ===~n", [Counter]),
    io:format("=== Temp list : ~p ===~n",[Temps]),
    io:format("=== Ultrasonic Ranges list : ~p ===~n",[Ranges]),
    {NewCounter, NewTempList, NewRangeList} = case Counter of
      20 ->
        io:format("=== Timer has ended, aggregating data and updating CRDT... === ~n"),
        AverageTemp = average(Temps),
        io:format("=== Average temp in past hour is ~p ===~n", [AverageTemp]),
        {ok, TempsCRDT} = lasp:query({<<"temp">>, state_orset}),
        TempsList = sets:to_list(TempsCRDT),
        % io:format("=== Temps CRDT : ~p ===~n", [TempsList]),
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
        {AnswerRange,Range} = node_sensor_server_worker:read(pmod_maxsonar),
        RangeList = case AnswerRange of
          read ->  lists:append(Ranges,[Range]);
          sensor_not_created -> exit(sensor_not_created)
        end,
        {Counter+1, TempList, RangeList}
    end,

    {noreply, S#state{counter=NewCounter, temps=NewTempList, ranges=NewRangeList}, 30000};

handle_info(Msg, State) ->
    io:format("=== Unknown message: ~p~n", [Msg]),
    {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(Reason, _S) ->
  io:format("=== Terminating Sensor client Gen Server (reason: ~p) ===~n",[Reason]),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.


%%====================================================================
%% Internal functions
%%====================================================================
average(List) -> sum(List)/length(List).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.
