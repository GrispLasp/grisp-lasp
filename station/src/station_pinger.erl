-module(station_pinger).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

start_link(Host) ->
    gen_server:start_link(?MODULE, {Host} , []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({Host}) ->
    process_flag(trap_exit, true),
    % Pong = net_adm:ping(Host),
    {ok, {Host}, 5000}.

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, {Host}) ->
    Pong = net_adm:ping(Host),
    io:format("Ping : ~p~n", [Pong]),
    {noreply, {Host}, 5000}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.
