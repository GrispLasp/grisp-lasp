-module(node_storage_util).

-include_lib("node.hrl").
-include_lib("lasp/include/lasp.hrl").

-compile({nowarn_unused_function}).

-compile({nowarn_export_all}).

-compile(export_all).

-define(NODES,  [node@Laymer, node2@Laymer]).
%% API
% -export([save_crdt/2,
%         load_crdt/1]).

%% Gen Server Callbacks

%% ===================================================================
%% API functions
%% ===================================================================

flush_crdt(Id, Filename) ->
  {ok, Set} = lasp:query(Id),
  L = sets:to_list(Set),
  {ok, {Iden, Type, Metadata, Val}} = lasp:update(Id, {rmv_all, L}, self()),
  ok = save_crdt(Id, Filename, {Iden, Type, Metadata, Val}),
  ets:delete(node(), Iden),
  {Iden, Type, Metadata, Val}.

save_crdt(Id, Filename, Var) ->
  Tmp = ets:new(Filename, [ordered_set, named_table, public]),
  % Var = ets:lookup(node(),Id),
  case ets:insert_new(Tmp, Var) of
    true ->
      case ets:tab2file(Tmp, Filename, [{sync, true}]) of
        ok ->
          lager:info("Saved CRDT ~p to file ~p ~n", [Id, Filename]),
          true = ets:delete(Tmp),
          % true = ets:delete(node(), Id),
          ok;
        {error, Reason} ->
          lager:error("Could not save ~p to SD card ~n", [Id]),
          {error, Reason}
      end;
    false ->
      lager:error("Could not insert ~p in tmp table ~n", [Var]),
      {error, insert}
  end.

load_crdt(Id, Filename) ->
  case ets:file2tab(Filename) of
    {ok, Tab} ->
      % All = ets:match(Filename, '$1'),
      L = ets:tab2list(Tab),
      case ets:insert_new(node(), L) of
        true ->
          lager:info("Loaded CRDT from file ~p ~n", [Filename]);
        false ->
          lager:info("Could not insert CRDT from file ~p ~n", [Filename])
          % Set = values_to_set(Id),
      end;
    {error, Reason} ->
      lager:error("Could not load CRDT from file ~p ~n", [Filename]),
      {error, Reason}
  end.

run_lasp() ->
  case application:ensure_all_started(lasp) of
    {ok, Deps} ->
      {ok, {Id, T, M, V}} = lasp:declare({<<"test">>, state_orset}, state_orset),
      lists:foreach(fun
        (Elem) ->
          lasp:update(Id, {add, Elem}, self())
      end, ["hello", "hi", "test"]),
      {ok, {Id, T, M, V}};
    _ ->
      error
    end.

test_remote() ->
  case application:ensure_all_started(lasp) of
    {ok, Deps} ->
      [Node] = ?NODES -- [node()],
      lager:info("Current node = ~p ~n", [Node]),
      case net_adm:ping(Node) of
        pong ->
          Nodes = nodes(),
          lager:info("Nodes = ~p ~n", [Nodes]),
          M = lasp_peer_service:manager(),
          Remote = rpc:call(Node, M, myself, []),
          ok = lasp_peer_service:join(Remote),
          {ok, Remote};
        pang ->
          ?PAUSE3,
          test_remote()
      end;
    _ ->
      error
  end.

test_remote(Node) ->
  case application:ensure_all_started(lasp) of
    {ok, Deps} ->
      case net_adm:ping(Node) of
        pong ->
          Nodes = nodes(),
          lager:info("Nodes = ~p ~n", [Nodes]),
          M = lasp_peer_service:manager(),
          Remote = rpc:call(Node, M, myself, []),
          ok = lasp_peer_service:join(Remote),
          {ok, Remote};
        pang ->
          ?PAUSE3,
          test_remote(Node)
        end;
    _ ->
      error
  end.

values_to_list(Id) ->
  {ok, Var} = lasp:query(Id),
  sets:to_list(Var).

values_to_set(Id) ->
  {ok, Var} = lasp:query(Id),
  Var.

match_crdt(Id, Tab) ->
  % [[{MatchedId, Data}] | Rest ] = ets:match(Tab, '$1').
  _DeltaVal = #dv{},
  L = ets:match(Tab, {Id, '$1'}),
  List = lists:flatten(L),
  List.
% node_storage_util:save_crdt({<<"temp">>, state_orset}, tempfile).
% node_storage_util:load_crdt(tempfile).
% lasp:query({<<"temp">>, state_orset}).
get_test() ->
  lasp:query({<<"test">>, state_orset}).

members() ->
  lasp_peer_service:members().

get_id() -> {<<"test">>, state_orset}.

look() ->
  ets:lookup(node(), get_id()).
