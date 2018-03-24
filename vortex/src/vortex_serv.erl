-module(vortex_serv).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


-define(WORKER_SPEC(MFA),
                    #{id => Name,
                    start => {vortex_worker_sup, start_link, [MFA]},
                    restart => temporary,
                    type => supervisor,
                    shutdown => 15000,
                    modules => [vortex_worker_sup]}).

-record(state, {limit=0,
                sup,
                refs,
                queue=queue:new()}).

start_link(Name, VortexSup, MFA) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, VortexSup, MFA}, []).

%% Gen server
init({Name, VortexSup, MFA}) ->
    self() ! {start_worker_supervisor, Name, VortexSup, MFA},
    {ok, #state{limit=10, refs=gb_sets:empty()}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_worker_supervisor, Name, VortexSup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(VortexSup, ?WORKER_SPEC(MFA)),
    link(Pid),
    {noreply, S#state{sup=Pid}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
