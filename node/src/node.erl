-module(node).

%% API
-export([start/0, start/1, start_node/0, start_partisan/0, start_lasp/0, stop_child/1, stop_server/0, start_worker/1, terminate_worker/1, kill_worker/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
  node_supersup:start_link().

start(all) ->
  % MACADDR = inet:ifget("wlan0", [hwaddr]),
  % io:format("MAC ADDR : ~p:~p:~p:~p:~p:~p ~n", MACADDR),
  io:format("Starting Partisan, Lasp and Node~n"),
  node_supersup:start_link(all).

start_node() ->
  io:format("Starting Node Supervisor~n"),
  node_supersup:start_node().

start_partisan() ->
  io:format("Starting Partisan~n"),
  node_supersup:start_partisan().

start_lasp() ->
  io:format("Starting Lasp~n"),
  node_supersup:start_lasp().

stop_child(Name) ->
  node_supersup:stop_child(Name).

stop_server() ->
  node_server:stop().

start_worker(WorkerType) ->
  node_server:start_worker(WorkerType).

terminate_worker(Pid) ->
  node_server:terminate_worker(Pid).

kill_worker(Pid) ->
  exit(Pid, kill).
