%%%-------------------------------------------------------------------
%% @doc node public API
%% @end
%%%-------------------------------------------------------------------

-module(node_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  io:format("Application Master has started app ~n"),
 %  T1 = os:timestamp(),
 % {ok, Supervisor} = node:start(all),
  {ok, Supervisor} = node:start(node),
  % {ok, Supervisor} = node:start(),
 %  T2 = os:timestamp(),
 %  Time = timer:now_diff(T2,T1),
 %  io:format("Time to start lasp partisan and node is ~p ~n",[Time/1000000]),
 %  LEDs = [1, 2],
 %  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
 %  PeerConfig = lasp_partisan_peer_service:manager(),
 %  io:format("The manager used is ~p ~n",[PeerConfig]),
  timer:sleep(5000),
  grisp:add_device(uart, pmod_maxsonar),
  timer:sleep(5000),
  grisp:add_device(spi1, pmod_gyro),
  timer:sleep(5000),
  grisp:add_device(spi2, pmod_als),
  timer:sleep(5000),
  {ok, Worker} = node_server:start_worker(node_stream_worker),
  timer:sleep(60000),
  display(Worker),

 %  node_server:start_worker(pinger_worker),
 %  timer:sleep(15000),
 %  node_server:start_worker(generic_tasks_server),
 %  timer:sleep(15000),
 %  node_server:start_worker(generic_tasks_worker),

  % node_server:start_worker(sensor_server_worker),
  % node_server:start_worker(sensor_client_worker),
  % node:start_all_workers(),
  {ok, Supervisor}.

stop(_State) ->
  io:format("Application Master has stopped app~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%
% display_data(Worker) ->
%     spawn_link(?MODULE, fun
%       (Streamer) when is_pid(Streamer) ->
%         display(Streamer)
%     end, [Worker]).

display(Worker) ->
    {ok, {Lum, Sonar, Gyro}} = gen_server:call(Worker, get_data),
    LumList = dict:to_list(Lum),
    io:format("Lum Data ~n"),
    erlang:rp(LumList),
    io:format("Raw Sonar Data ~n"),
    erlang:rp(Sonar),
    io:format("Raw Gyro Data ~n"),
    erlang:rp(Gyro),
    timer:sleep(60000),
    display(Worker).
