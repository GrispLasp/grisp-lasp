-module(node_server).
-behaviour(gen_server).

%% API
-export([start_link/1, start_worker/1, terminate_worker/1, stop/0]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Macros
-define(NODE_WORKER_SUP_SPEC,
                    #{id => node_worker_sup,
                    start => {node_worker_sup, start_link, []},
                    restart => temporary,
                    type => supervisor,
                    shutdown => 15000,
                    modules => [node_worker_sup]}).

-define(CLUSTER_SPEC,
                    #{id => cluster_server,
                        start => {node_clustering_worker, start_link, []},
                        restart => permanent,
                        type => worker,
                        shutdown => brutal_kill,
                        modules => [node_clustering_worker]}).

%% Records
-record(state, {worker_sup,
                workers}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(NodeSup) ->
  gen_server:start_link({local, node_server}, ?MODULE, {NodeSup}, []).

stop() ->
  gen_server:call(node_server, stop).

start_worker(WorkerType) ->
  gen_server:call(node_server, {start_worker, WorkerType}).

terminate_worker(Pid) ->
  gen_server:call(node_server, {terminate_worker, Pid}).

%% ===================================================================
%% Private functions
%% ===================================================================

get_worker_specs_map() ->
  #{cluster_worker => ?CLUSTER_SPEC}.

%% ===================================================================
%% Gen Server callbacks
%% ===================================================================


init({NodeSup}) ->
    io:format("Initializing Node Server~n"),
    process_flag(trap_exit, true), %% Ensure Gen Server gets notified when his supervisor dies
    self() ! {start_worker_supervisor, NodeSup},
    {ok, #state{workers=gb_sets:empty()}}.


handle_call({start_worker, WorkerType}, _From, S = #state{worker_sup=WorkerSup, workers=W}) ->
    io:format("=== Starting new worker (~p) ===~n", [WorkerType]),
    case maps:get(WorkerType, get_worker_specs_map()) of
      {badkey, _} ->
        io:format("=== Worker Type not found in map ===~n"),
        {reply, {badkey, worker_type_not_exist}, S};
      ChildSpec ->
        io:format("=== Found Worker Spec ~p === ~n",[ChildSpec]),
        {ok, Pid} = supervisor:start_child(WorkerSup, ChildSpec),
        Ref = erlang:monitor(process, Pid),
        {reply, {ok,Pid}, S#state{workers=gb_sets:add(Ref,W)}}
    end;

handle_call({terminate_worker, WorkerPid}, _From, S = #state{worker_sup=WorkerSup}) ->
    io:format("=== Terminate worker (~p) ===~n", [WorkerPid]),
    case supervisor:terminate_child(WorkerSup, WorkerPid) of
      {error, not_found} ->
        io:format("=== Worker with PID ~p was not found in the worker supervisor (pid: ~p) ===", [WorkerPid, WorkerSup]),
        {reply, {error, pid_not_found, WorkerPid}, S};
      ok ->
          {reply, {ok, killed, WorkerPid}, S}
    end;

handle_call(stop, _From, S) ->
  {stop, normal, ok, S};

handle_call(_Msg, _From, S) ->
  {noreply, S}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info({start_worker_supervisor, NodeSup}, S = #state{}) ->
    io:format("=== Start Node Worker Supervisor ===~n"),
    {ok, WorkerSupPid} = supervisor:start_child(NodeSup, ?NODE_WORKER_SUP_SPEC),
    link(WorkerSupPid),
    io:format("=== PID of Node Worker Supervisor ~p ===~n", [WorkerSupPid]),
    {noreply, S#state{worker_sup=WorkerSupPid}};

handle_info({'DOWN', Ref, process, Pid, Info}, S = #state{workers=Refs}) ->
    io:format("=== Worker ~p is dead (because of ~p), removing him from workers set ===~n", [Pid, Info]),
    case gb_sets:is_element(Ref, Refs) of
        true ->
            erlang:demonitor(Ref),
            {noreply, S#state{workers=gb_sets:delete(Ref,Refs)}};
        false ->
            {noreply, S}
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
