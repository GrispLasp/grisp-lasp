-module(node_utils_server).

-behaviour(gen_server).

%% API
-export([start_link/0, terminate/0]).
-export([get_cpu_usage/0]).

%% Gen Server Callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-record(samples_state, {
   s1 = [],
	 sysload = 0.0
}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

get_cpu_usage() -> gen_server:call(?MODULE, {get_cpu_usage}).

terminate() -> gen_server:call(?MODULE, {terminate}).

%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init([]) ->
    io:format("Starting a node utility server ~n"),
		S1 = scheduler:sample_all(),
		State = #samples_state{s1 = S1},
    erlang:send_after(5000, self(), {get_cpu_usage}),
		% {ok, State, 5000}.
		{ok, State}.

% handle_call({get_cpu_usage}, _From, State = #samples_state{s1 = S1}) ->
% 		S2 = scheduler:sample_all(),
% 		Schedulers = node_util:utilization_sample(S1,S2),
% 		[Total|Scheds] = Schedulers,
% 		io:format("=== Getting first CPU usage: ~p ===~n", [Total]),
% 		% io:format("=== Getting CPU usage since last util() call: ~p ===~n", [cpu_sup:util()]),
% 		{total, Load, Percentage} = Total,
% 		NewState = #samples_state{s1 = S2, sysload = Load * 100},
% 		% cpu_sup:util(),
% 		{reply, get_cpu_usage, NewState};

handle_call({get_cpu_usage}, _From, State = #samples_state{s1 = S1, sysload = Load}) ->
  	{reply, {ok, Load}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

% handle_info(timeout, State = #samples_state{s1 = S1}) ->
% 		S2 = scheduler:sample_all(),
% 		Schedulers = node_util:utilization_sample(S1,S2),
% 		[Total|Scheds] = Schedulers,
% 		io:format("=== Getting first CPU usage: ~p ===~n", [Total]),
%     % io:format("=== Getting CPU usage since last util() call: ~p ===~n", [cpu_sup:util()]),
% 		{total, Load, Percentage} = Total,
% 		NewState = #samples_state{s1 = S2, sysload = Load * 100},
% 		% cpu_sup:util(),
%     {reply, get_cpu_usage, NewState};
%
handle_info({get_cpu_usage}, State = #samples_state{s1 = S1, sysload = Load}) ->
		S2 = scheduler:sample_all(),
		[Total|Schedulers] = node_util:utilization_sample(S1,S2),
		io:format("=== Getting CPU usage since last util() call: ~p ===~n", [Total]),
		{total, NewLoad, Percentage} = Total,
		NewState = #samples_state{s1 = S2, sysload = NewLoad * 100},
    {noreply, NewState};

handle_info(Msg, State) ->
    io:format("=== Unknown message: ~p~n", [Msg]),
    {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(Reason, _S) ->
    io:format("=== Terminating node utily server (reason: ~p) ===~n",[Reason]),
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%====================================================================
%% Internal functions
%%====================================================================
