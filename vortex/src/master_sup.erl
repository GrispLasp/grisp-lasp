% @doc board top level supervisor.
% @end
-module(master_sup).

-behavior(supervisor).

% API
-export([start_link/0, start_partisan/0, start_vortex/2, stop/0, stop_vortex/1]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, vortex}, ?MODULE, []).

stop() ->
    case whereis(vortex) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

start_vortex(Name, MFA) ->
    ChildSpec = #{id => Name,
                        start => {vortex_sup, start_link, [Name, MFA]},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [vortex_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, VortexSup} = supervisor:start_child(vortex, ChildSpec),
    {ok, VortexSup}.

start_partisan() ->
    ChildSpec = #{id => partisan_sup,
                        start => {partisan_sup, start_link, []},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [partisan_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, PartisanSup} = supervisor:start_child(vortex, ChildSpec),
    {ok, PartisanSup}.

stop_vortex(Name) ->
    supervisor:terminate_child(vortex, Name),
    supervisor:delete_child(vortex, Name).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, { {one_for_all, 1, 5}, []} }.
