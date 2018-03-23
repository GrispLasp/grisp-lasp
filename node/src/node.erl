% @doc node public API.
% @end
-module(node).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
  {ok, Supervisor} = node_sup:start_link(),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
  timer:sleep(5000),
  grisp_led:off(2),
  Random = fun() ->
      {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
  end,
  % discover_nodes(),
  % partisan_default_peer_service_manager:start_link(),
  % {ok, Hyparview} = partisan_hyparview_peer_service_manager:start_link(),
  % timer:sleep(5000),
  % timer:sleep(5000),
  % partisan_config:set(partisan_peer_service_manager, partisan_hyparview_peer_service_manager),
  ok = partisan_config:init(),
  timer:sleep(5000),
  Self = partisan_peer_service_manager:myself(),
  timer:sleep(5000),
  pong = ping_station(),
  % rpc:call(node@my_grisp_board, node, start, [a,b]).
  % timer:sleep(5000),
  % {ok, LaspSup} = lasp_sup:start_link(),
  % timer:sleep(5000),
  % ok = lasp_peer_service:join('station@Macbook-Pro'),

  % timer:sleep(5000),
  % {ok, CRDT} = lasp:query({<<"set">>, state_orset}),
  % partisan_config:set(peer_ip, {192,168,0,149}),
  grisp_led:pattern(1, [{100, Random}]),
  % {ok, Supervisor, CRDT}.
  {ok, Supervisor}.
  % {ok, Supervisor, Hyparview, LaspSup}.

stop(_State) -> ok.

ping_station() ->
  Ping = net_adm:ping('station@MacBook-Pro'),
  if
   Ping == pong ->
      pong;
   Ping == pang ->
      timer:sleep(1000),
      ping_station()
  end.
