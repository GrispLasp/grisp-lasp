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

%% test call to Numerix
-export([myfit/0]).



%%====================================================================
%% API
%%====================================================================
% TODO : find a way to exclude lager everywhere?, see commit below
% https://github.com/lasp-lang/lasp/pull/295/commits/e2f948f879145a5ff31cf5458201768ca97b406b

start(_StartType, _StartArgs) ->
    io:format("Application Master starting Node app ~n"),
    {ok, Supervisor} = node:start(node),
    % application:ensure_all_started(os_mon),
    node_util:set_platform(),

    start_timed_apps(),

    io:format("Application Master started Node app ~n"),
    start_primary_workers(primary_workers),
    start_primary_workers(distributed_workers),
    add_measurements(),

    % Adding a new task in Lasp :
    node_generic_tasks_server:add_task({task1, all, fun () -> node_generic_tasks_functions:temp_sensor({0, []}, 3000) end }),
    node_generic_tasks_worker:start_task(task1),

    LEDs = [1, 2],
    [grisp_led:flash(L, aqua, 500) || L <- LEDs],

    PeerConfig = lasp_partisan_peer_service:manager(),
    io:format("The manager used is ~p ~n", [PeerConfig]),

    {ok, Supervisor}.

%%--------------------------------------------------------------------

stop(_State) ->
    io:format("Application Master has stopped app~n"), ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_primary_workers(Workers) ->
    PrimaryWorkers = node_config:get(Workers, []),
    lists:foreach(fun(Worker) ->
                    node_server:start_worker(Worker)
                  end, PrimaryWorkers),
    node_util:printer(PrimaryWorkers, workers).

add_measurements() ->
    Measurements = node_config:get(node_sensor_server_worker_measurements, []),
    lists:foreach(fun(Type) ->
      node_sensor_server_worker:creates(Type)
    end, Measurements),
    node_util:printer(Measurements, measurements).

%% https://github.com/SpaceTime-IoT/erleans/blob/5ee956c3bc656558d56e611ca2b8b75b33ba0962/src/erleans_app.erl#L46
start_timed_apps() ->
  Apps = node_config:get(timed_apps, []),
  T1 = erlang:monotonic_time(second),
  Started = lists:foldl(fun(App, Acc) ->
                  case application:ensure_all_started(App) of
                      {ok, Deps} ->
                          [Deps | Acc];
                      {error, Reason} ->
                          logger:error("Could not start application
                            ~s: reason=~p", [App, Reason]),
                          Acc
                  end
                end, [], Apps),
              T2 = erlang:monotonic_time(second),
              Time = T2 - T1,
              io:format("Time to start ~p"
              "is approximately ~p seconds ~n",
              [Started, Time]).


%%====================================================================
%% Useful snippets
%%====================================================================

myfit() ->
  {Intercept, Slope} = 'Elixir.Numerix.LinearRegression':fit([1.3, 2.1, 3.7, 4.2], [2.2, 5.8, 10.2, 11.8]),
  {Intercept, Slope}.

% Adding a new task in Lasp :
% node_generic_tasks_server:add_task({task1, all, fun () -> node_generic_tasks_functions:temp_sensor({0, []}, 3000) end }),
% node_generic_tasks_worker:start_task(task1),

% Generate mock temperature measurements
% node_sensor_server_worker:creates(temp),
