-module(vortex_worker_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(MFA = {_,_,_}) ->
    supervisor:start_link(?MODULE, MFA).

init({M,F,A}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => MaxRestart,
                 period => MaxTime},
    ChildSpecs = [#{id => vortex_worker,
                    start => {M,F,A},
                    restart => temporary,
                    type => worker,
                    shutdown => 5000,
                    modules => [M]}],
    ok = supervisor:check_childspecs(ChildSpecs),
    {ok, {SupFlags, ChildSpecs}}.
