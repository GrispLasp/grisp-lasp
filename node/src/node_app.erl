%%%-------------------------------------------------------------------
%% @doc node public API
%% @end
%%%-------------------------------------------------------------------

-module(node_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  io:format("Application Master has started app ~n"),
  T1 = os:timestamp(),
 {ok, Supervisor} = node:start(all),
  T2 = os:timestamp(),
  Time = timer:now_diff(T2,T1),
  io:format("Time to start lasp partisan and node is ~p ~n",[Time/1000000]),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
%  grisp_led:off(2),
%  Random = fun() ->
%      {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
%  end,
%  grisp_led:pattern(1, [{100, Random}]),
  PeerConfig = lasp_partisan_peer_service:manager(),
  io:format("The manager used is ~p ~n",[PeerConfig]),
  automatic_join([],5),
  Pid = spawn(client_sensor,init,[]),
  io:format("Creating sensor client at ~p ~n",[Pid]),
  {ok, Supervisor}.

stop(_State) ->
  io:format("Application Master has stopped app~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
automatic_join(PingList,N) when N > 0->
    List = [node@my_grisp_board_1,node@my_grisp_board_2,node@my_grisp_board_3,node@my_grisp_board_4,node@my_grisp_board_5,node@my_grisp_board_6,node@my_grisp_board_7,node@my_grisp_board_8,node@my_grisp_board_9,node@my_grisp_board_10,node@my_grisp_board_11,node@my_grisp_board_12],
    ListWithoutSelf = lists:delete(node(),List),
    Ping = fun(X) ->
      net_adm:ping(X) == pong
    end,
    ListToJoin = lists:filter(Ping, ListWithoutSelf),
    if length(ListToJoin) > length(PingList) -> grisp_led:flash(1, red, 500), automatic_join(ListToJoin,5);
        true ->  grisp_led:flash(1, blue, 500), automatic_join(ListToJoin,N-1)
      end;
automatic_join(PingList,0) ->
  grisp_led:color(1,green),
  Join = fun(X) ->
   lasp_peer_service:join(X)
  end,
  lists:foreach(Join,PingList),
  io:format("list to join is ~p ~n",[PingList]).
