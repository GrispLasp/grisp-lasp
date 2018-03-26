% @doc station public API.
% @end
-module(station).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

-export([gen_compute/1]).
%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
  {ok, Supervisor} = master_sup:start_link(),
  {ok, _} = master_sup:start_station(pinger, {station_pinger, start_link, []}),
  {ok, _} = master_sup:start_partisan(),
  {ok, _} = master_sup:start_lasp(),
  timer:sleep(5000),
  % {ok, {Id, Type, Metadata, Value}} = lasp:declare({<<"set">>, state_orset}, state_orset),
  % {ok, V} = lasp:query({<<"results">>, state_orset}).
  {ok, {Id, _, _, _}} = lasp:declare({<<"data">>, state_orset}, state_orset),
  timer:sleep(5000),
  Computation = gen_compute(factorial(10)),
  % Binary = term_to_binary(Computation),
  % lasp:update({<<"functions">>, state_orset}, {add, {Computation}}, self()),
  lists:foreach(fun(Count) -> lasp:update(Id, {add, Count}, self()) end, lists:seq(1,50)),
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

factorial(N) ->
  factorial(1, N, 1).

factorial(Current, N, Result) when Current =< N ->
  NewResult = Result * Current,
  factorial(Current+1, N, NewResult);

factorial(_, _, Result) ->
  Result.

gen_compute(Fun) ->
  fun() -> Fun() end.

% {env, [{_, _, _, Abs}]} = erlang:fun_info(Fun, env).
% Str = erl_pp:expr({'fun', 1, {clauses, Abs}}).
% io:format([Str|"\n"]).
