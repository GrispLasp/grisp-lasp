-module(node_clustering_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, rejoin_cluster/0]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  gen_server:start_link({local, cluster_server}, ?MODULE, {} , []).

stop() ->
  gen_server:call(cluster_server, stop).

rejoin_cluster() ->
    gen_server:call(cluster_server, start_clustering).

-record(state, {}).

%% ===================================================================
%% Private functions
%% ===================================================================


%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init(_Args) ->
    io:format("Initializing Clustering Gen Server~n"),
    process_flag(trap_exit, true),
    self() ! start_clustering,
    {ok, {}}.

handle_info(start_clustering, S) ->
  lists:foreach(
    fun (Node) ->
      case lasp_peer_service:join(Node) of
           ok ->
             io:format("=== Joined Lasp cluster on ~p ===~n", [Node]);
           {error, Reason} ->
             io:format("=== Did not connect to ~p (reason: ~p) ===~n", [Node, Reason])
      end
    end, nodes()),
    {noreply, S};

handle_info(Msg,  S) ->
  io:format("=== Got a message ~p ===~n",[Msg]),
  {noreply, S}.

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.
