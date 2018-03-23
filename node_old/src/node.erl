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
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    timer:sleep(5000),
    grisp_led:off(2),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    ok = partisan_config:init(),
    % partisan_config:set(partisan_peer_service_manager, partisan_hyparview_peer_service_manager),
    partisan_sup:start_link(),
    lasp_sup:start_link(),
    % partisan_config:set(peer_ip, {192,168,1,11}),
    % partisan_hyparview_peer_service_manager:start_link(),
    % partisan_default_peer_service_manager:start_link(),
    grisp_led:pattern(1, [{100, Random}]),
    % application:load(partisan),
    % partisan_sup:start_link(),
    % partisan_default_peer_service_manager:start_link(),
    % partisan_default_peer_service_manager:myself(),
    % partisan_peer_service_manager:myself(),
    % partisan_hyparview_peer_service_manager:myself().
    % partisan_peer_service:members().
    % partisan_peer_service:join(lasp1@dan).
    % lasp_peer_service:join(#{name => 'lasp1@dan', listen_addrs => [#{ip => {192,168,1,3}, port => 4369}]}).
    % -internal_epmd epmd_sup
    % net_adm:ping(lasp1@dan).
    {ok, Supervisor}.

stop(_State) -> ok.
