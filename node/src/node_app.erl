%%%-------------------------------------------------------------------
%% @doc node application public API
%% @end
%%%-------------------------------------------------------------------

% /!\ NOTE :
% 3.1 Timer Module
% Creating timers using erlang:send_after/3 and erlang:start_timer/3 , is much more efficient than using the timers provided by the timer module in STDLIB.
% The timer module uses a separate process to manage the timers.
% That process can easily become overloaded if many processes create and cancel timers frequently (especially when using the SMP emulator).
% The functions in the timer module that do not manage timers (such as timer:tc/3 or timer:sleep/1),
% do not call the timer-server process and are therefore harmless.

-module(node_app).

-behaviour(application).

-include("node.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-compile({nowarn_unused_function}).

%%====================================================================
%% API
%%====================================================================
% TODO : find a way to get rid of lager, see commit below
% https://github.com/lasp-lang/lasp/pull/295/commits/e2f948f879145a5ff31cf5458201768ca97b406b

start(_StartType, _StartArgs) ->
    io:format("Application Master starting Node app ~n"),
    % {ok, Supervisor} = node:start(node),
    case os:type() of % Check if application is ran on a grisp or a laptop
      {unix, darwin} -> os:putenv("type", "laptop");
      {unix, linux} -> os:putenv("type", "laptop");
      _ -> os:putenv("type", "grisp")
    end,
    T1 = erlang:monotonic_time(second),
    % {ok, Supervisor} = node:start(all),
<<<<<<< HEAD
    {ok, Supervisor} = node:start(node),
=======
  	{ok, _Started} = application:ensure_all_started(lasp),
>>>>>>> adcccf9d1f7c2d857da54ed6311c52dfa4e1e5f6
    T2 = erlang:monotonic_time(second),
    Time = T2 - T1,
    io:format("Time to start lasp partisan and node "
	      "is approximately ~p seconds ~n",
	      [Time]),

    {ok, Supervisor} = node:start(node),
    io:format("Application Master started Node app ~n"),
    LEDs = [1, 2],
    [grisp_led:flash(L, aqua, 500) || L <- LEDs],
    PeerConfig = lasp_partisan_peer_service:manager(),
    io:format("The manager used is ~p ~n", [PeerConfig]),
    % ?PAUSE10,
<<<<<<< HEAD
    % ?PAUSE10,
    node_server:start_worker(pinger_worker),
    % ?PAUSE10,
    % ?PAUSE10,
    node_server:start_worker(generic_tasks_server),
    % ?PAUSE10,
    % ?PAUSE10,
    node_server:start_worker(generic_tasks_worker),
    % ?PAUSE10,
    % ?PAUSE10,
    node_server:start_worker(sensor_server_worker),
    node_sensor_server_worker:creates(temp),
    % ?PAUSE10,
    % ?PAUSE10,
=======
    % ?PAUSE10,
    % node_server:start_worker(pinger_worker),
    % % ?PAUSE10,
    % ?PAUSE10,
    % node_server:start_worker(generic_tasks_server),
    % % ?PAUSE10,
    % ?PAUSE10,
    % node_server:start_worker(generic_tasks_worker),
    % % ?PAUSE10,
    % ?PAUSE10,
    % node_server:start_worker(sensor_server_worker),
    % node_sensor_server_worker:creates(temp),
    % % ?PAUSE10,
    % ?PAUSE10,
>>>>>>> adcccf9d1f7c2d857da54ed6311c52dfa4e1e5f6
    % grisp:add_device(spi2, pmod_als),
    % ?PAUSE10,
    % ?PAUSE10,
    % {ok, _Worker} =	node_server:start_worker(node_stream_worker),
    % ?PAUSE10,
    % ?PAUSE10,
    % node_generic_tasks_server:add_task({task1, all, fun () -> node_generic_tasks_functions:temp_sensor({0, []}, 3000) end }),
    % node_generic_tasks_worker:start_task(task1),
    % node_sensor_server_worker:creates(temp),
    % ?PAUSE10,
    % ?PAUSE10,
    % run(),
    {ok, Supervisor}.

%%--------------------------------------------------------------------

stop(_State) ->
    io:format("Application Master has stopped app~n"), ok.

%%====================================================================
%% Internal functions
%%====================================================================

% run() -> spawn_link(fun () -> process(1) end).
run() -> spawn_link(fun () -> node_generic_tasks_worker:leds() end).

%%--------------------------------------------------------------------

process(N) ->
    ?PAUSEHMIN,
    Epoch = (?HMIN) * N,
    io:format("Data after = ~p seconds ~n", [?TOS(Epoch)]),
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
printer([H], Arg) ->
    io:format("Elem = ~p ~n", [H]),
    io:format("done printing ~p ~n", [Arg]);
printer([H | T], Arg) ->
    ?PAUSEMS,
    io:format("Elem = ~p ~n", [H]),
    printer(T, Arg).
