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
  {ok, Supervisor} = node:start(all),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
  timer:sleep(5000),
  grisp_led:off(2),
  Random = fun() ->
      {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
  end,
  grisp_led:pattern(1, [{100, Random}]),
  % {ok, Supervisor} = node_sup:start_link(),
  % node:start_node(),
  {ok, Supervisor}.

stop(_State) ->
  io:format("Application Master has stopped app~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
