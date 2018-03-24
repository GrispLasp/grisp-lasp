% @doc vortex public API.
% @end
-module(vortex).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
  {ok, Supervisor} = master_sup:start_link(),
  % {ok, VortexSupervisor} = master_sup:start_vortex(vortex_pinger, {vortex_pinger, start_link, ['vortex@my_grisp_board']}),
  % {ok, PartisanSupervisor} = master_sup:start_partisan(),
  {ok, _} = master_sup:start_vortex(vortex_pinger, {vortex_pinger, start_link, ['vortex@my_grisp_board']}),
  {ok, _} = master_sup:start_partisan(),
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
