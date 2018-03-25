% @doc vortex public API.
% @end
-module(vortex).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

% Cannot reference remote anonymous functions
% see : https://github.com/elixir-lang/elixir/issues/1520
-export([gen_compute/1]).
%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
  {ok, Supervisor} = master_sup:start_link(),
  % {ok, VortexSupervisor} = master_sup:start_vortex(vortex_pinger, {vortex_pinger, start_link, ['vortex@my_grisp_board']}),
  % {ok, PartisanSupervisor} = master_sup:start_partisan(),
  % {ok, _} = master_sup:start_vortex(vortex_pinger, {vortex_pinger, start_link, ['vortex@my_grisp_board']}),
  {ok, _} = master_sup:start_vortex(pinger, {vortex_pinger, start_link, []}),
  {ok, _} = master_sup:start_partisan(),
  {ok, _} = master_sup:start_lasp(),
  {ok, _} = vortex_serv:run(pinger, ['station@Laymer']),
  timer:sleep(5000),

  lasp_peer_service:join('station@Laymer'),
  timer:sleep(5000),

  {ok, {_, _, _, _}} = lasp:declare({<<"results">>, state_orset}, state_orset),
  timer:sleep(5000),

  master_sup:stop_vortex(pinger),
  timer:sleep(5000),
  {ok, _} = master_sup:start_vortex(computation, {vortex_generic_computation, start_link, []}),
  timer:sleep(5000),
  {ok, _} = vortex_serv:run(computation, [{<<"functions">>, state_orset}]),
  timer:sleep(5000),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
  timer:sleep(5000),
  grisp_led:off(2),
  Random = fun() ->
      {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
  end,
  grisp_led:pattern(1, [{100, Random}]),
  {ok, Supervisor}.

stop(_State) -> ok.
% partisan_peer_service_manager:myself().
% lasp_peer_service:join('station@Laymer').
gen_compute(Fun) ->
  Fun().
