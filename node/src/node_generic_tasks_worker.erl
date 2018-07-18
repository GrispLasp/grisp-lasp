-module(node_generic_tasks_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, find_and_start_task/0, start_task/1, start_all_tasks/0, isRunning/1, stop/0]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Records
-record(state, {running_tasks, finished_tasks}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

start_task(Name) ->
  gen_server:call(?MODULE, {start_task, Name}).

find_and_start_task() ->
  gen_server:call(?MODULE, {find_and_start_task}).

start_all_tasks() ->
  gen_server:call(?MODULE, {start_all_tasks}).

isRunning(TaskName) ->
  gen_server:call(?MODULE, {isRunning, TaskName}).

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
    erlang:send_after(5000, self(), {start_all_tasks}),
    {ok, #state{running_tasks=[], finished_tasks=[]}}.



handle_call({start_task, Name}, _From, State = #state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}) ->
    io:format("=== State is ~p ===~n", [State]),
    io:format("=== Finding task ~p ===~n", [Name]),
		CanRunTask = can_run_task(length(RunningTasks)),
		case CanRunTask of
			true ->
		    Task = node_generic_tasks_server:find_task(Name),
		    case Task of
		      {ok, TaskFound} ->
		        NewFinishedTasksList = FinishedTasks -- [TaskFound],
		        TaskFun = element(3,TaskFound),
		        io:format("=== Task chosen ~p ===~n", [TaskFound]),
		        {Pid, Ref} = spawn_monitor(TaskFun),
		        io:format("=== Spawned Task fun : PID ~p - Ref ~p ===~n", [Pid, Ref]),
		        RunningTask = erlang:insert_element(4, TaskFound, {Pid, Ref}),
		        io:format("=== Running Task ~p ===~n", [RunningTask]),
		        {reply, RunningTask,  State#state{running_tasks=RunningTasks ++ [RunningTask], finished_tasks=NewFinishedTasksList}};
		      Error ->
		        {reply, Error, State}
		    end;
			false ->
				io:format("=== Cannot run task, device is overloaded ===~n"),
				{reply, ko, State}
			end;


handle_call({find_and_start_task}, _From, State = #state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}) ->
    io:format("=== State is ~p ===~n", [State]),
    io:format("=== Finding new task ===~n"),
    TasksList = node_generic_tasks_server:get_all_tasks(),
    io:format("=== Tasks list ~p ===~n", [TasksList]),
    FilteredTaskList = filter_task_list(TasksList, RunningTasks),
    io:format("=== FilteredTaskList ~p ===~n",[FilteredTaskList]),
    case length(FilteredTaskList) of
      0 ->
        {reply, no_tasks_to_run, State};
      _ ->
        RandomTaskIndex = rand:uniform(length(FilteredTaskList)),
        RandomTask = lists:nth(RandomTaskIndex, FilteredTaskList),
        CanRunTask = can_run_task(length(RunningTasks)),
        case CanRunTask of
          true ->
            NewFinishedTasksList = FinishedTasks -- [RandomTask],
            TaskFun = element(3,RandomTask),
            io:format("=== Task chosen ~p ===~n", [RandomTask]),
            {Pid, Ref} = spawn_monitor(TaskFun),
            io:format("=== Spawned Task fun : PID ~p - Ref ~p ===~n", [Pid, Ref]),
            RunningTask = erlang:insert_element(4, RandomTask, {Pid, Ref}),
            io:format("=== Running Task ~p ===~n", [RunningTask]),
            {reply, RunningTask, State#state{running_tasks=RunningTasks ++ [RunningTask], finished_tasks=NewFinishedTasksList}};
          false ->
				    io:format("=== Cannot run task, device is overloaded ===~n"),
            {reply, ko, State#state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}}
        end
    end;


handle_call({isRunning, TaskName}, _From, State = #state{running_tasks=RunningTasks, finished_tasks=_}) ->
  TaskRunning = [{Name, Targets, Fun, {TaskPid, TaskRef}} || {Name, Targets, Fun, {TaskPid, TaskRef}} <- RunningTasks, Name =:= TaskName],
  case length(TaskRunning) of
    0 -> {reply, false, State};
    1 -> {reply, true, State};
    _ -> {reply, more_than_one_task, State}
  end;




handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({start_all_tasks}, State = #state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}) ->
    case start_all_tasks_periodically(RunningTasks, FinishedTasks) of
      {ko, no_tasks_to_run} ->
        io:format("=== No tasks to run ===~n"),
        {noreply, State#state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}, 60000};
      {NewRunningTasksList, NewFinishedTasksList} ->
        {noreply, State#state{running_tasks=RunningTasks ++ NewRunningTasksList, finished_tasks=NewFinishedTasksList}, 60000}
      end;


handle_info(timeout, State = #state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}) ->
  case start_all_tasks_periodically(RunningTasks, FinishedTasks) of
    {ko, no_tasks_to_run} ->
      io:format("=== No tasks to run ===~n"),
      {noreply, State#state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}, 60000};
    {NewRunningTasksList, NewFinishedTasksList} ->
      {noreply, State#state{running_tasks=RunningTasks ++ NewRunningTasksList, finished_tasks=NewFinishedTasksList}, 60000}
    end;



handle_info({'DOWN', Ref, process, Pid, Info}, State = #state{running_tasks=RunningTasks, finished_tasks=FinishedTasks}) ->
    io:format("== Pid ~p has ended ===~n", [Pid]),
    RunningTasksList = [{Name, Targets, Fun, {TaskPid, TaskRef}} || {Name, Targets, Fun, {TaskPid, TaskRef}} <- RunningTasks, TaskPid =:= Pid],
    case length(RunningTasksList) of
      0 ->
        io:format("=== A process other than a task finished ===~n"),
        {noreply, State};
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
        {noreply, State#state{running_tasks=NewRunningTasksList, finished_tasks=NewFinishedTasksList}}
    end;

handle_info({'EXIT', _From, Reason}, State) ->
    io:format("=== Supervisor sent an exit signal (reason: ~p), terminating Gen Server ===~n", [Reason]),
    {stop, Reason, State};

handle_info(Msg, State) ->
  io:format("=== Unknown message: ~p~n", [Msg]),
  {noreply, State}.

terminate(normal, _State) ->
  io:format("=== Normal Gen Server termination ===~n"),
  ok;

terminate(shutdown, _State) ->
  io:format("=== Supervisor asked to terminate Gen Server (reason: shutdown) ===~n"),
  ok;

terminate(Reason, _State) ->
  io:format("=== Terminating Gen Server (reason: ~p) ===~n",[Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

get_cpu_load() ->
	Load = cpu_sup:avg1(),
	PercentLoad = 100 * (1 - 50/(50 + Load)).

get_device() ->
	os:getenv("type").

can_run_task(RunningTasksCount) ->
	CpuLoad = get_cpu_load(),
	io:format("=== CPU load ~.3f ===~n",[CpuLoad]),
	DeviceType = get_device(),
	io:format("=== Device is ~p ===~n",[DeviceType]),
	TresholdReached = case DeviceType of
		"grisp" ->
			if RunningTasksCount =< 2 -> false;
				true -> true
			end;
		"laptop" ->
			if RunningTasksCount =< 5 -> false;
				true -> true
			end
	end,
	io:format("=== Is threshold reached? ~p ===~n",[TresholdReached]),
	CanRun = if CpuLoad < 50, TresholdReached =:= false -> true;
		true -> false
	end,
	CanRun.


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

start_all_tasks_periodically(RunningTasks, FinishedTasks) ->
  io:format("=== Finding new task ===~n"),
  TasksList = node_generic_tasks_server:get_all_tasks(),
  io:format("=== Tasks list ~p ===~n", [TasksList]),
  FilteredTaskList = filter_task_list(TasksList, RunningTasks),
  case length(FilteredTaskList) of
    0 ->
      {ko, no_tasks_to_run};
    _ ->
      NewFinishedTasksList = FinishedTasks -- FilteredTaskList,
      StartedTasks = lists:mapfoldl(
        fun(Task, StartedTasks) ->
          CanRunTask = can_run_task(length(RunningTasks)),
          case CanRunTask of
      			true ->
              TaskFun = element(3, Task),
              io:format("=== Task chosen ~p ===~n", [Task]),
              {Pid, Ref} = spawn_monitor(TaskFun),
              io:format("=== Spawned Task fun : PID ~p - Ref ~p ===~n", [Pid, Ref]),
              RunningTask = erlang:insert_element(4, Task, {Pid, Ref}),
              io:format("=== Running Task ~p ===~n", [RunningTask]),
              {Task, StartedTasks ++ RunningTask};
            false ->
			        io:format("=== Cannot run task, device is overloaded ===~n"),
              {Task, StartedTasks}
          end
        end
      , [], FilteredTaskList),
      NewRunningTasksList = element(2, StartedTasks),
      {NewRunningTasksList, NewFinishedTasksList}
end.
