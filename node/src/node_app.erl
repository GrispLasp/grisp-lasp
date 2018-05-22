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
  % setup(),
  % Devices = grisp:devices(),
  % io:format("Devices : ~p ~n",Devices),
  % io:fwrite("~w~n",[Devices]),
  % run(),
  % T1 = os:timestamp(),
 % {ok, Supervisor} = node:start(all),
 {ok, Supervisor} = node:start(),
  % T2 = os:timestamp(),
  % Time = timer:now_diff(T2,T1),
  % io:format("Time to start lasp partisan and node is ~p ~n",[Time/1000000]),
  application:set_env(grisp, devices, [{spi1, pmod_gyro}]),
  application:set_env(grisp, devices, [{uart, pmod_maxsonar}]),
  grisp_devices:setup(),
  pmod_gyro:read_gyro(),
  pmod_gyro:read_temp(),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
  % PeerConfig = lasp_partisan_peer_service:manager(),
  % io:format("The manager used is ~p ~n",[PeerConfig]),
  % timer:sleep(5000),
  % node_server:start_worker(pinger_worker),
  % timer:sleep(5000),
  % node_server:start_worker(generic_worker),
  % timer:sleep(5000),
  % node_server:start_worker(sensor_server_worker),
  % timer:sleep(5000),
  % node_server:start_worker(sensor_client_worker),
  % node:start_all_workers(),
  {ok, Supervisor}.

stop(_State) ->
  io:format("Application Master has stopped app~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

setup() ->
    application:set_env(grisp, drivers, [
        {spi, grisp_spi_drv}
        % {spi, grisp_spi_drv_emu}
    ]),
    application:set_env(grisp, devices, [
        {spi1, pmod_acl2}
    ]),
    ok.
    % {ok, Apps} = application:ensure_all_started(grisp),

    % Devices = grisp:devices(),
    % Apps.
    % Devices.

run() ->
    spawn_link(fun() -> process() end).

process() ->
    {X, Y, Z} = pmod_acl2:g(),
    Color = {color(X), color(Y), color(Z)},
    grisp_led:color(1, Color),
    grisp_led:color(2, Color),
    timer:sleep(10),
    process().

color(Val) -> trunc((abs(Val) / 2.0) + 0.8) rem 2.
