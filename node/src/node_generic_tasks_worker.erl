-module(node_generic_tasks_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, find_and_start_task/0, stop/0]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Records
-record(state, {running_tasks, finished_tasks}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

find_and_start_task() ->
  gen_server:call(?MODULE, {find_and_start_task}).

stop() ->
  gen_server:call(?MODULE, stop).

%% ===================================================================
%% Private functions
%% ===================================================================


%% ===================================================================
%% Gen Server callbacks
%% ===================================================================


init({}) ->
    io:format("Initializing Node Server~n"),
    {ok, #state{running_tasks=[], finished_tasks=[]}}.


handle_call({find_and_start_task}, _From, S = #state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}) ->
    io:format("=== State is ~p ===~n", [S]),
    io:format("=== Finding new task ===~n"),
    TasksList = node_generic_tasks_server:get_all_tasks(),
    io:format("=== Tasks list ~p ===~n", [TasksList]),
    FilteredTaskList = filter_task_list(TasksList, RunningTasks),
    io:format("=== FilteredTaskList ~p ===~n",[FilteredTaskList]),
    case length(FilteredTaskList) of
      0 ->
        {reply, {no_tasks_to_run}, S};
      _ ->
        RandomTaskIndex = rand:uniform(length(FilteredTaskList)),
        RandomTask = lists:nth(RandomTaskIndex, FilteredTaskList),
        NewFinishedTasksList = case lists:member(RandomTask, FinishedTasks) of
          true ->
            lists:delete(RandomTask, FinishedTasks);
          false ->
            FinishedTasks
        end,
        TaskFun = element(3,RandomTask),
        io:format("=== Task chosen ~p ===~n", [RandomTask]),
        {Pid, Ref} = spawn_monitor(TaskFun),
        io:format("=== Spawned Task fun : PID ~p - Ref ~p ===~n", [Pid, Ref]),
        RunningTask = erlang:insert_element(4, RandomTask, {Pid, Ref}),
        io:format("=== Running Task ~p ===~n", [RunningTask]),
        {reply, RunningTask, S#state{running_tasks=RunningTasks ++ [RunningTask], finished_tasks=NewFinishedTasksList}}
  end;



handle_call(stop, _From, S) ->
  {stop, normal, ok, S};

handle_call(_Msg, _From, S) ->
  {noreply, S}.

handle_cast(_Msg, S) ->
  {noreply, S}.


handle_info({'DOWN', Ref, process, Pid, Info}, S = #state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}) ->

    RunningTasksList = [{Name, Targets, Fun, {TaskPid, TaskRef}} || {Name, Targets, Fun, {TaskPid, TaskRef}} <- RunningTasks, TaskPid =:= Pid],
    case length(RunningTasksList) of
      0 ->
        io:format("=== A process other than a task finished ===~n"),
        {noreply, S};
      1 ->
        {Name, Targets, Fun, {TaskPid, TaskRef}} = hd(RunningTasksList),
        case Info of
          normal -> io:format("=== Task ~p with Pid ~p finished gracefully (~p) ===~n", [Name, Pid, Info]);
          _ -> io:format("=== Problem: ~p ===~n", [Info])
        end,
        erlang:demonitor(Ref),
        NewRunningTasksList = lists:delete({Name, Targets, Fun, {TaskPid, TaskRef}}, RunningTasks),
        NewFinishedTasksList = lists:append(FinishedTasks, [{Name, Targets, Fun}]),
        io:format("=== NRTL ~p , NFTL ~p ===~n", [NewRunningTasksList, NewFinishedTasksList]),
        {noreply, S#state{running_tasks=NewRunningTasksList, finished_tasks=NewFinishedTasksList}}
    end;

handle_info({'EXIT', _From, Reason}, S) ->
    io:format("=== Supervisor sent an exit signal (reason: ~p), terminating Gen Server ===~n", [Reason]),
    {stop, Reason, S};

handle_info(Msg, S) ->
  io:format("=== Unknown message: ~p~n", [Msg]),
  {noreply, S}.

terminate(normal, _S) ->
  io:format("=== Normal Gen Server termination ===~n"),
  ok;

terminate(shutdown, _S) ->
  io:format("=== Supervisor asked to terminate Gen Server (reason: shutdown) ===~n"),
  ok;

terminate(Reason, _S) ->
  io:format("=== Terminating Gen Server (reason: ~p) ===~n",[Reason]),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.


%%====================================================================
%% Internal functions
%%====================================================================


filter_task_list(TasksList, RunningTasks) ->
  FilteredTaskList = lists:filter(
  fun ({Name, Targets, _}) ->
    IsTarget = case Targets of
      all -> true;
      List -> lists:member(node(), List)
    end,
    TaskIsRunning = case length(RunningTasks) of
      0 -> false;
      _ ->
        lists:any(
        fun({ProcessingTaskName, _, _, _}) ->
          if ProcessingTaskName =:= Name -> false;
            true -> true
          end
        end, RunningTasks)
    end,
    io:format("=== Task is already running : ~p - Node is target : ~p ===~n", [TaskIsRunning, IsTarget]),
    IsCanditate = if TaskIsRunning =:= false, IsTarget =:= true -> true;
       true -> false
    end,
    IsCanditate
  end , TasksList),
  FilteredTaskList.
