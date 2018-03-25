% @doc board top level supervisor.
% @end
-module(master_sup).

-behavior(supervisor).

% API
-export([start_link/0, start_partisan/0, start_station/2, stop/0, stop_station/1, start_lasp/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, station}, ?MODULE, []).

stop() ->
    case whereis(station) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

start_station(Name, MFA) ->
    ChildSpec = #{id => Name,
                        start => {station_sup, start_link, [Name, MFA]},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [station_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, StationSup} = supervisor:start_child(station, ChildSpec),
    {ok, StationSup}.

start_partisan() ->
    partisan_config:set(partisan_peer_service_manager, partisan_hyparview_peer_service_manager),
    ChildSpec = #{id => partisan_sup,
                        start => {partisan_sup, start_link, []},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [partisan_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, PartisanSup} = supervisor:start_child(station, ChildSpec),
    {ok, PartisanSup}.

start_lasp() ->
    ChildSpec = #{id => lasp_sup,
                        start => {lasp_sup, start_link, []},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [lasp_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, PartisanSup} = supervisor:start_child(station, ChildSpec),
    {ok, PartisanSup}.

stop_station(Name) ->
    supervisor:terminate_child(station, Name),
    supervisor:delete_child(station, Name).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, { {one_for_all, 1, 5}, []} }.
