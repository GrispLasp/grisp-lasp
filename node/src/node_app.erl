%%%-------------------------------------------------------------------
%% @doc node application public API
%% @end
%%%-------------------------------------------------------------------

-module(node_app).

-behaviour(application).

-include("node.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  io:format("Application Master has started app ~n"),
  % {ok, Supervisor} = node:start(node),
  T1 = os:timestamp(),
  {ok, Supervisor} = node:start(all),
  T2 = os:timestamp(),
  % ?PAUSE10,
  % {ok, _Worker} = node_server:start_worker(node_stream_worker_emu),
  Time = timer:now_diff(T2,T1),
  io:format("Time to start lasp partisan and node is ~p ~n",[Time/1000000]),

  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],

  PeerConfig = lasp_partisan_peer_service:manager(),
  io:format("The manager used is ~p ~n",[PeerConfig]),

  ?PAUSE10,
  node_server:start_worker(pinger_worker),
  ?PAUSE10,
  % node_server:start_worker(generic_tasks_server),
  % ?PAUSE10,
  % node_server:start_worker(generic_tasks_worker),
  % ?PAUSE10,
  % node_server:start_worker(sensor_server_worker),

  ?PAUSE10,
  grisp:add_device(uart, pmod_maxsonar),

  ?PAUSE10,
  grisp:add_device(spi1, pmod_gyro),

  ?PAUSE10,
  grisp:add_device(spi2, pmod_als),

  ?PAUSE10,
  {ok, _Worker} = node_server:start_worker(node_stream_worker),
  ?PAUSE10,
  run(),
  {ok, Supervisor}.

%%--------------------------------------------------------------------

stop(_State) ->
  io:format("Application Master has stopped app~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

run() ->
  spawn_link(fun() -> process(1) end ).

%%--------------------------------------------------------------------

process(N) ->
  ?PAUSEHMIN,
  Epoch = ?HMIN * N,
  io:format("Data after = ~p seconds ~n",[?TOS(Epoch)]),

  {ok, Lum} = lasp:query({<<"als">>, state_orset}),
  ?PAUSE3,
  LumList = sets:to_list(Lum),
  ?PAUSE3,
  {ok, MS} = lasp:query({<<"maxsonar">>, state_orset}),
  Sonar = sets:to_list(MS),
  ?PAUSE3,
  {ok, Gyr} = lasp:query({<<"gyro">>, state_orset}),
  Gyro = sets:to_list(Gyr),

  io:format("Raw ALS Data ~n"),
  printer(LumList, luminosity),

  io:format("Raw Sonar Data ~n"),
  printer(Sonar, sonar),

  io:format("Raw Gyro Data ~n"),
  printer(Gyro, gyro),

  process(N + 1).

%%--------------------------------------------------------------------

printer([], Arg) ->
  io:format("nothing left to print for ~p ~n", [Arg]);

printer([H|[]], Arg) ->
  io:format("Elem = ~p ~n", [H]),
  io:format("done printing ~p ~n", [Arg]);

printer([H|T], Arg) ->
  ?PAUSEMS,
  io:format("Elem = ~p ~n", [H]),
  printer(T, Arg).
