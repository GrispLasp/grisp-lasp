% @doc robot public API.
% @end
-module(robot).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    {ok, Supervisor} = robot_sup:start_link(),
    ok = partisan_config:init(),
    {ok, PartisanSup} = partisan_sup:start_link(),
    {ok, LaspSup} = lasp_sup:start_link(),
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    timer:sleep(5000),
    grisp_led:off(2),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    lasp:declare({<<"set">>, state_orset}, state_orset),
    % {ok, {Id, Type, Metadata, Value}} = lasp:declare({<<"set">>, state_orset}, state_orset),
    SelfRecord = partisan_peer_service_manager:myself(),
    lasp:update({<<"set">>, state_orset}, {add, SelfRecord}, self()),
    % pattern becomes observable on the board once the Lasp suite has been initialized
    % {ok, {Id, Type, Metadata, Value}}.
    {ok, Value1} = lasp:query({<<"set">>, state_orset}), sets:to_list(Value1),
    grisp_led:pattern(1, [{100, Random}]),
    % {ok, Value1}.
    {ok, Value1, Supervisor, PartisanSup, LaspSup}.

stop(_State) -> ok.
