-module(node_utils_server).

-behaviour(gen_server).

%% API
-export([start_link/0, terminate/0]).

%% Gen Server Callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

terminate() -> gen_server:call(?MODULE, {terminate}).

%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init([]) ->
    io:format("Starting a node utility server ~n"),
    erlang:send_after(5000, self(), {get_cpu_usage}),
    {ok, {}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_info(timeout, State) ->
    io:format("=== Getting CPU usage since last util() call: ~p ===~n", [cpu_sup:util()]),
    cpu_sup:util(),
    {noreply, State, 5000};

handle_info({get_cpu_usage}, State) ->
    io:format("=== Getting first CPU usage: ~p ===~n", [cpu_sup:util()]),
    {noreply, State, 5000};

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
