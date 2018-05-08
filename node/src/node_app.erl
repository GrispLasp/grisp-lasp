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
  T1 = os:timestamp(),
 {ok, Supervisor} = node:start(all),
  T2 = os:timestamp(),
  Time = timer:now_diff(T2,T1),
  io:format("Time to start lasp partisan and node is ~p ~n",[Time/1000000]),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],

  PeerConfig = lasp_partisan_peer_service:manager(),
  io:format("The manager used is ~p ~n",[PeerConfig]),
  node_server:start_worker(pinger_worker),
  node_server:start_worker(sensor_server_worker),
  node_server:start_worker(sensor_client_worker),
  % node:start_all_workers(),
  {ok, Supervisor}.

stop(_State) ->
  io:format("Application Master has stopped app~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
