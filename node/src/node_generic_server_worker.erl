-module(node_generic_server_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, terminate/0]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, set_fun/1, set_fun/0, get_fun/0]).

%% Records
% -record(state, {functions}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

terminate() -> gen_server:call(?MODULE, {terminate}).

set_fun(GenFun) -> gen_server:call(?MODULE, {set_gen_fun, GenFun}).

set_fun() -> gen_server:call(?MODULE, {set_gen_fun}).

get_fun() -> gen_server:call(?MODULE, {get_gen_fun}).


%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init([]) ->
  io:format("Starting a generic function server ~n"),
  {ok, {}, 20000}.

handle_call({set_gen_fun, GenFun}, _From, State) ->
  {ok, Functions} = lasp:query({<<"functions">>, state_orset}),
  FunctionsList = sets:to_list(Functions),
  case length(FunctionsList) of
    0 ->
      io:format("=== No other function is present in the CRDT, adding new gen fun ===~n"),
      lasp:update({<<"functions">>, state_orset}, {add, GenFun}, self());
    1 ->
      io:format("=== Another function is present in the CRDT, removing old fun and adding new one ===~n"),
      OldFun = hd(FunctionsList),
      lasp:update({<<"functions">>, state_orset}, {rmv, OldFun}, self());
    _ ->
      io:format("=== More then one function is present in the CRDT, a new function has been added recently
      and the previous one has not yet been removed, waiting for CRDT convergence ===~n")
  end,
  {reply, ok, State, 5000};

handle_call({set_gen_fun}, _From, State) ->
  InverseHyperbolicArctanFun = fun() -> io:format("atanh = ~f~n", [math:atanh(1 - (1/math:pow(6,20)))]) end,
  lasp:update({<<"functions">>, state_orset}, {add, InverseHyperbolicArctanFun}, self()),
  io:format("=== Added Hyperbolic Arctangent Function ===~n"),
  {reply, ok, State, 5000};

handle_call({get_gen_fun}, _From, State) ->
  Function = get_gen_fun(),
  {reply, Function, State, 5000};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.


handle_info(timeout, State) ->
    io:format("=== Retrieving generic fun and executing it locally ===~n"),
    Fun = get_gen_fun(),
    case Fun of
      ko ->
        io:format("=== No generic function found to execute ===~n");
      _ ->
        Fun()
    end,
    {noreply, State, 20000};

handle_info(Msg, State) ->
    io:format("=== Unknown message: ~p~n", [Msg]),
    {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(Reason, _S) ->
  io:format("=== Terminating Generic server (reason: ~p) ===~n",[Reason]),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.



%%====================================================================
%% Internal functions
%%====================================================================


get_gen_fun() ->
  {ok, Functions} = lasp:query({<<"functions">>, state_orset}),
  FunctionsList = sets:to_list(Functions),
  Function = case length(FunctionsList) of
    1 ->
      io:format("=== Gen function is present in the CRDT ===~n"),
      hd(FunctionsList);
    _ ->
      io:format("=== No gen function present or convergence not yet achieved ===~n"),
      ko
  end.
