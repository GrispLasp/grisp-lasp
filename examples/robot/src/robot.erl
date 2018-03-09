% @doc robot public API.
% @end
-module(robot).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    % [Local, Enp, Bcast] = element(2, inet:getifaddrs()),
    % [_, Enp, _] = element(2, inet:getifaddrs()),
    % _ = element(2, Enp),
    % Interface = element(2, Enp),
    % {addr, Address} = lists:keyfind(addr, 1, Interface),
    {ok, Supervisor} = robot_sup:start_link(),

    LEDs = [1, 2],
    % [grisp_led:flash(L, red, 500) || L <- LEDs],
    [grisp_led:color(L, red) || L <- LEDs],

    % timer:sleep(5000),
    % {ok, Hostname} = inet:gethostname(),
    % {ok, Address} = inet:getaddr(Hostname, inet),
    % [_, Nodename] = string:tokens(atom_to_list(node()), "@"),
    % {ok, NodeAddress} = inet:getaddr(Nodename, inet),
    ok = partisan_config:init(),
    grisp_led:color(1, green),
    {ok, PartisanSup} = partisan_sup:start_link(),
    grisp_led:color(2, green),
    {ok, LaspSup} = lasp_sup:start_link(),
    [grisp_led:color(K, magenta) || K <- LEDs],

    % Nodes = discover_nodes(),

    % grisp_led:off(2),
    % Random = fun() ->
    %     {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    % end,
    lasp:declare({<<"set">>, state_orset}, state_orset),
    grisp_led:color(1, yellow),
    % {ok, {Id, Type, Metadata, Value}} = lasp:declare({<<"set">>, state_orset}, state_orset),
    SelfRecord = partisan_peer_service_manager:myself(),
    lasp:update({<<"set">>, state_orset}, {add, SelfRecord}, self()),
    grisp_led:color(2, yellow),
    % pattern becomes observable on the board once the Lasp suite has been initialized
    % {ok, {Id, Type, Metadata, Value}}.
    {ok, Value1} = lasp:query({<<"set">>, state_orset}), sets:to_list(Value1),
    [grisp_led:color(M, blue) || M <- LEDs],
    % grisp_led:pattern(1, [{100, Random}]),
    % {ok, Value1}.
    % {ok, Value1, Supervisor, PartisanSup, LaspSup, Address, NodeAddress, Nodes}.
    {ok, Value1, Supervisor, PartisanSup, LaspSup}.

stop(_State) -> ok.

% discover_nodes() ->
%     Epmd = (net_kernel:epmd_module()),
%     {ok, Subnets} = inet:getif(),
%     lists:map(fun(Subnet) ->
%
%       lists:map(fun(Address) ->
%           Epmd:names(Address)
%       end, tuple_to_list(Subnet))
%
%     end,
%     Subnets).
