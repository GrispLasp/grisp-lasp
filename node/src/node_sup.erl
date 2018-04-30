% @doc node top level supervisor.
% @end
-module(node_sup).

-behavior(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, node_sup}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_all,
               intensity => 1,
               period => 20},
  ChildSpecs = [#{id => my_sensor,
                  start => {my_sensor, start_link, []},
                  restart => permanent,
                  type => worker,
                  shutdown => 5000,
                  modules => [my_sensor]}],
  {ok, {SupFlags, ChildSpecs}}.
