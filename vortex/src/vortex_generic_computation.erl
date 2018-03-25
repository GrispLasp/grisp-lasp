-module(vortex_generic_computation).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {vars}).

start_link(Computation) ->
    gen_server:start_link(?MODULE, {Computation} , []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({Computation}) ->
    process_flag(trap_exit, true),
    self() ! {compute, Computation},
    {ok, #state{vars=gb_sets:empty()}}.

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({compute, {Set, Type}}, S = #state{vars=V}) ->
    io:format("Received ~w~n",[Type]),
    {ok, Funs} = lasp:query({Set, Type}),
    [B] = sets:to_list(Funs),
    % TODO register station process for on-demand remote computations on all nodes in edge
    % F = binary_to_term(B),
    % F(),
    lasp:update({<<"results">>, state_orset}, {add, {B}}, self()),
    % lasp:query({<<"functions">>, state_orset}).
    {noreply, S#state{vars=gb_sets:add(Set,V)}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.
% {env, [{_, _, _, Abs}]} = erlang:fun_info(D, env).
