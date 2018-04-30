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
  {ok, Supervisor} = node:start(all),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
  timer:sleep(5000),
  grisp_led:off(2),
  Random = fun() ->
      {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
  end,
  grisp_led:pattern(1, [{100, Random}]),
  % {ok, Supervisor} = node_sup:start_link(),
  % node:start_node(),
  {ok, Supervisor}.

stop(_State) ->
  io:format("Application Master has stopped app~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
automatic_join() ->
    % List = [node@my_grisp_board_3],
    List = [node@my_grisp_board_1,
    node@my_grisp_board_2,
    node@my_grisp_board_3,
    node@my_grisp_board_4,
    node@my_grisp_board_5,
    node@my_grisp_board_6,
    node@my_grisp_board_7,
    node@my_grisp_board_8,
    node@my_grisp_board_9,
    node@my_grisp_board_10,
    node@my_grisp_board_11,
    node@my_grisp_board_12,],
    
    Ping = fun(X) ->
      net_adm:ping(X) == pong
    end,
    ListToJoin = lists:filter(Ping, List),
    if length(ListToJoin) > 0 -> grisp_led:color(1,blue),automatic_join();
        true -> grisp_led:color(1,red),automatic_join()
      end.
  %  Join = fun(X) ->
  %    lasp_peer_service:join(X)
  %  end,
  %  lists:foreach(Join,ListToJoin),
  %  io:format("list to join is ~p ~n",[ListToJoin]),
  %List = [node@my_grisp_board,node@my_grisp_board_2,node@my_grisp_board_3,node@my_grisp_board_4,node@my_grisp_board_5],
  %lists:foreach(Join,List),
  %{ok,Members} = lasp_peer_service:members(),
  %io:format("The list of nodes in members i ~p ~n",[Members]).
