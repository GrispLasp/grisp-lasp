%%%-------------------------------------------------------------------
%% @author Igor Kopestenski <i.kopest@gmail.com>
%%   [https://github.com/Laymer/GrispLasp]
%% @doc This is a <em>simulation sensor data stream</em> module.
%% @end
%%%-------------------------------------------------------------------

-module(node_stream_worker).

-behaviour(gen_server).

-include("node.hrl").

%% API
-export([get_data/0, refresh_webserver/0, start_link/0,
	 start_link/1]).

%% Gen Server Callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-compile({nowarn_unused_function, [{stream_data, 2}]}).

-compile(export_all).

%%====================================================================
%% Macros
%%====================================================================

-define(PMOD_ALS_RANGE, lists:seq(1, 255, 1)).

-define(PMOD_ALS_REFRESH_RATE, ?TEN).

-define(PMOD_MAXSONAR_REFRESH_RATE, ?TEN).

-define(PMOD_GYRO_REFRESH_RATE, ?TEN).

%%====================================================================
%% Records
%%====================================================================

% NOTE : prepend atom to list to avoid ASCII representation of integer lists
% -record(state,
% 	{luminosity = [lum], sonar = [son], gyro = [gyr]}).
-record(state,
	{luminosity = [], sonar = [], gyro = []}).
% -record(state,
% 	{luminosity = #{}, sonar = [], gyro = []}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

start_link(Mode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Mode},
			  []).

get_data() -> gen_server:call(?MODULE, {get_data}).

refresh_webserver() ->
    'Webserver.NodeClient':get_nodes().

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init({Mode}) ->
    % Shades = lists:duplicate(255, #shade{}),
    % Range = (?PMOD_ALS_RANGE),
    % List = lists:zipwith(fun (X, Y) -> {X, Y} end, Range,
	% 		 Shades),
    % Dict = dict:from_list(List),
    % State = #state{luminosity = Dict},
	% Map = maps:new(),
	% NewMap = maps:put(luminosity, Dict, Map),
    % State = #state{luminosity = Dict},
	_ = rand:seed(exsp),

    State = #state{},
    case Mode of
      emu ->
	  io:format("Starting Emulated stream worker ~n"),
	  % grisp:remove_device(spi1, pmod_gyro),
	  % grisp:add_device(spi1, pmod_nav),
	  % AllRegs = [{Comp, Reg} || {Comp, Regs} <- maps:to_list(pmod_nav:registers()), Reg <- maps:to_list(Regs)],
	  % [ pmod_nav:read(Comp, [Reg]) || {Comp, {Reg, {_Addr, _Type, _Size, _Conv}}} <- AllRegs ],
	  flood(),
	  {ok, State};
      board ->
	  io:format("Starting stream worker on GRiSP ~n"),
	  {ok, State, 10000};
	  % {ok, NewMap, 10000};
      _ -> {stop, unknown_launch_mode}
    end.

%%--------------------------------------------------------------------

handle_call({Request}, _From,
	    State = #state{luminosity = Lum, sonar = Sonar,
			   gyro = Gyro}) ->
    case Request of
      % guard clause for snippet
      get_data when is_atom(get_data) ->
	  {reply, {ok, {Lum, Sonar, Gyro}},
	   State = #state{luminosity = Lum, sonar = Sonar,
			  gyro = Gyro}};
      _ -> {reply, unknown_call, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------

handle_info(timeout,
	    State = #state{luminosity = Lum, sonar = Sonar,
			   gyro = Gyro}) ->
    % Raw = pmod_als:raw(),
    % RawSonar = pmod_maxsonar:get(),
    % RawGyro = pmod_gyro:read_gyro(),
	Raw = rand:uniform(255),
    RawSonar = rand:uniform(100),
    RawGyro = {rand:uniform(100),rand:uniform(100),rand:uniform(100)},
    % RawSonar = 100,
    % RawGyro = 200,
    % NewLum = dict:update(Raw,
	% 		 fun (Shade) ->
	% 			 #shade{measurements =
	% 				    Shade#shade.measurements ++ [Raw],
	% 				count = Shade#shade.count + 1}
	% 		 end,
	% 		 Lum),
	% NewLum = #{Raw => 1},
	% NewLum = Lum:put(){Raw => 1},
	% NewLum = maps:put(Raw, 1, Lum),
	NewLum = Lum ++ [Raw],
    NewSonar = Sonar ++ [RawSonar],
    NewGyro = Gyro ++ [RawGyro],
    % NewSonar = Sonar,
    % NewGyro = Gyro,
    % ok = stream_data(?PMOD_ALS_REFRESH_RATE, als),
    % ok = stream_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar),
    % ok = stream_data(?PMOD_GYRO_REFRESH_RATE, gyro),
    % ok = store_data(?PMOD_ALS_REFRESH_RATE, als, dict:to_list(NewLum), node(), self(), atom_to_binary(als, latin1)),
    % ok = store_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar, NewSonar, node(), self(), atom_to_binary(maxsonar, latin1)),
    % ok = store_data(?PMOD_GYRO_REFRESH_RATE, gyro, NewGyro, node(), self(), atom_to_binary(gyro, latin1)),
    % State = #state{luminosity = NewLum, sonar = NewSonar,
	% 	   gyro = NewGyro},
	NewState = #state{luminosity = NewLum, sonar = NewSonar,
		   gyro = NewGyro},
    % State#state{luminosity = NewLum, sonar = NewSonar,
	% 	   gyro = NewGyro},
		%
		% Fields = record_info(fields, state),
		% KVElems = lists:map(fun(X) -> {X, []} end, Fields),
		% Map = maps:from_list(KVElems),
		% maps:update(luminosity, NewLum, Map),
		% maps:update(sonar, NewSonar, Map),
		% maps:update(gyro, NewGyro, Map),
    ok = store_state(?PMOD_GYRO_REFRESH_RATE, states, NewState,
		     node(), self()),
    % ok = store_state(?PMOD_GYRO_REFRESH_RATE, states, #state{}, node(), self()),
    {noreply, NewState};
handle_info(states,
	    State = #state{luminosity = Lum, sonar = Sonar,
			   gyro = Gyro}) ->
    % Raw = pmod_als:raw(),
    % RawSonar = pmod_maxsonar:get(),
    % RawGyro = pmod_gyro:read_gyro(),
	Raw = rand:uniform(255),
    RawSonar = rand:uniform(100),
    RawGyro = {rand:uniform(100),rand:uniform(100),rand:uniform(100)},
    % NewLum = dict:update(Raw,
	% 		 fun (Shade) ->
	% 			 #shade{measurements =
	% 				    Shade#shade.measurements ++ [Raw],
	% 				count = Shade#shade.count + 1}
	% 		 end,
	% 		 Lum),
	% NewLum = maps:put(Raw, 1, Lum),
	% NewLum = maps:put(Raw, 1, Lum),
	NewLum = Lum ++ [Raw],
    NewSonar = Sonar ++ [RawSonar],
    NewGyro = Gyro ++ [RawGyro],
    % ok = stream_data(?PMOD_GYRO_REFRESH_RATE, gyro),
    NewState = #state{luminosity = NewLum, sonar = NewSonar,
		   gyro = NewGyro},
	% State#state{luminosity = NewLum, sonar = NewSonar,
	% 	   gyro = NewGyro},
	 	% maps:update("a", 42, Map).
		% Map = maps:new(),
		%
		% % maps:put("a", 42, Map).
		% maps:put(luminosity, NewLum, Map),
		% maps:put(sonar, NewSonar, Map),
		% maps:put(gyro, NewGyro, Map),
    ok = store_state(?PMOD_GYRO_REFRESH_RATE, states, NewState,
		     node(), self()),
    % ok = store_state(?PMOD_GYRO_REFRESH_RATE, states, Map,
	% 	     node(), self()),
    % {noreply, State};
    {noreply, NewState};
handle_info(als,
	    State = #state{luminosity = Lum, sonar = Sonar,
			   gyro = Gyro}) ->
    Raw = pmod_als:raw(),
    NewLum = dict:update(Raw,
			 fun (Shade) ->
				 #shade{measurements =
					    Shade#shade.measurements ++ [Raw],
					count = Shade#shade.count + 1}
			 end,
			 Lum),
    % ok = stream_data(?PMOD_ALS_REFRESH_RATE, als),
    ok = store_data(?PMOD_ALS_REFRESH_RATE, als,
		    dict:to_list(NewLum), node(), self(),
		    atom_to_binary(gyro, latin1)),
    {noreply,
     State#state{luminosity = NewLum, sonar = Sonar,
		 gyro = Gyro}};
handle_info(maxsonar,
	    State = #state{luminosity = Lum, sonar = Sonar,
			   gyro = Gyro}) ->
    RawSonar = pmod_maxsonar:get(),
    NewSonar = Sonar ++ [RawSonar],
    % ok = stream_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar),
    ok = store_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar,
		    NewSonar, node(), self(), atom_to_binary(gyro, latin1)),
    {noreply,
     State#state{luminosity = Lum, sonar = NewSonar,
		 gyro = Gyro}};
handle_info(gyro,
	    State = #state{luminosity = Lum, sonar = Sonar,
			   gyro = Gyro}) ->
    RawGyro = pmod_gyro:read_gyro(),
    NewGyro = Gyro ++ [RawGyro],
    % ok = stream_data(?PMOD_GYRO_REFRESH_RATE, gyro),
    ok = store_data(?PMOD_GYRO_REFRESH_RATE, gyro, NewGyro,
		    node(), self(), atom_to_binary(gyro, latin1)),
    {noreply,
     State#state{luminosity = Lum, sonar = Sonar,
		 gyro = NewGyro}};
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

stream_data(Rate, Sensor) ->
    erlang:send_after(Rate, self(), Sensor), ok.

%%--------------------------------------------------------------------
store_data(Rate, Type, SensorData, Node, Self,
	   BitString) ->
    ?PAUSE10,
    {ok, Set} = lasp:query({BitString, state_orset}),
    L = sets:to_list(Set),
    case length(L) of
      1 ->
	  % H = hd(L),
	  lasp:update({BitString, state_orset}, {rmv, {Node, L}},
		      Self),
	  lasp:update({BitString, state_orset},
		      {add, {Node, SensorData}}, Self);
      0 ->
	  lasp:update({BitString, state_orset},
		      {add, {Node, SensorData}}, Self),
	  ok;
      _ -> ok
    end,
    % lasp:update({BitString, state_orset}, {add, {Node, SensorData}}, Self),
    erlang:send_after(Rate, Self, Type),
    ok.

% node_stream_worker:store_state(5000, states, {state,[3],[c],[{lol},{jk}]}, node(), self()).
store_state(Rate, Type, State, Node, Self) ->
    ?PAUSE10,
  	% io:format("State ~p ~n", [State]),
    BitString = atom_to_binary(Type, latin1),
    {ok, Set} = lasp:query({BitString, state_orset}),
    L = sets:to_list(Set),
    MapReduceList = lists:filtermap(fun (Elem) ->
					    case Elem of
					      {Node,
					       _S = #state{luminosity = _Lum,
							   sonar = _Sonar,
							   gyro = _Gyro}} ->
						  {true, {Node, State}};
					      _ -> false
					    end
				    end,
				    L),

		% Map = maps:new(),
		%
		% % maps:put("a", 42, Map).
		% maps:put(luminosity, State#state.luminosity, Map),
		% maps:put(sonar, State#state.sonar, Map),
		% maps:put(gyro, State#state.gyro, Map),
		% % lists:foreach(fun(Field) -> maps:put(Field, State#state.luminosity, Map) end, record_info(fields, state)),
    case length(L) of
      0 ->
	  lasp:update({BitString, state_orset},
		      {add, {Node, State}}, Self),
		      % {add, {Node, Map}}, Self),
	  ok;
      % 1 when length(MapReduceList) > 0 ->
      1 ->
	  Leaving = hd(L),
	  % io:format("Leaving : ~p ~n", [Leaving]),
	  H = hd(MapReduceList),
	  % io:format("MapReduce Head : ~p ~n", [H]),
	  lasp:update({BitString, state_orset}, {rmv, Leaving},
		      Self),
	  lasp:update({BitString, state_orset}, {add, H}, Self);
      _ -> ok
    end,
    % lasp:update({BitString, state_orset}, {add, {Node, SensorData}}, Self),
    erlang:send_after(Rate, Self, Type),
    ok.

flood() ->
    List = lists:seq(1, 30000, 1),
    Fun = fun (Elem) when is_integer(Elem) ->
		  ?PAUSE1,
		  lasp:update({<<"flood">>, state_orset},
			      {add, {node(), Elem}}, self())
	  end,
    ok = lists:foreach(Fun, List).

% lasp:query({<<"als">>, state_orset}).
% lasp:query({<<"sonar">>, state_orset}).
% lasp:query({<<"gyro">>, state_orset}).

% BS = <<"set">>.
% lasp:update({BS, state_orset}, {add, {node(), [1,2,3]}}, self()).
% ALS =
% lasp:update({BS, state_orset}, {add, {node(), [1,2,3]}}, self()).
% {ok, Set} = lasp:query({BS, state_orset}).
% {ok, ALSet} = lasp:query({<<"als">>, state_orset}).
% AList = sets:to_list(ALSet).
% AList = [{node@board, [{64,{shade,[],0}}, {243,{shade,[],0}}, {179,{shade,[],0}}, {115,{shade,[],0}}, {51,{shade,[],0}}]}].
% ALSList = [{64,{shade,[],0}}, {243,{shade,[],0}}, {179,{shade,[],0}}, {115,{shade,[],0}}, {51,{shade,[],0}}], SonarList = lists:seq(1, 10, 1), GyroList = lists:seq(1, 20, 1).
% State = #state{luminosity = ALSList, sonar = SonarList, gyro = GyroList}.
% ALSDict = dict:from_list(ALSList).

% State = #state{luminosity = dict:to_list(ALSDict), sonar = SonarList, gyro = GyroList}.
% State2 = #state{luminosity = ALSList, sonar = SonarList, gyro = GyroList}.
% State11 = #state{luminosity = dict:to_list(ALSDict), sonar = SonarList ++ [8,8,8], gyro = GyroList ++ [9,9,9]}.
% rd(state, {luminosity = [], sonar = [], gyro = []}).
% StateBS = <<"states">>.

% lasp:update({<<"states">>, state_orset}, {add, {node(), {state, ALSList = [{64,{shade,[],0}}, {243,{shade,[],0}}, {179,{shade,[],0}}, {115,{shade,[],0}}, {51,{shade,[],0}}], SonarList = lists:seq(1, 10, 1), GyroList = lists:seq(1, 20, 1)}}}, self()).
% lasp:update({<<"states">>, state_orset}, {add, {node(), {state, [{64,{shade,[],0}}], lists:seq(1, 5, 1), lists:seq(1, 10, 1)}}}, self()).
% lasp:update({StateBS, state_orset}, {add, {node(), State}}, self()).
% lasp:update(StateId, {add, {node(), State}}, self()).
% lasp:update({StateBS, state_orset}, {add, {node@my_grisp_board_11, State11}}, self()).
% {ok, StateSet} = lasp:query({StateBS, state_orset}).
% StateId = {StateBS, state_orset}.
% StateId = {<<"states">>, state_orset}.
% lasp:filter(StateId, fun(X) -> X rem 2 == 0 end, StateId).

% Node = node@board.
% lasp:update({StateBS, state_orset}, {add, {Node, State2}}, self()).
% lasp:update({StateBS, state_orset}, {add, {stuff, State2}}, self()).

% SourceId = {<<"states">>, state_orset}.
% lasp:update(StateId, {add, {node@board2, State}}, Pid).
% lasp:query({<<"states">>, state_orset}).
% sets:to_list(lists:nth(2, lists:flatten(tuple_to_list(lasp:query({<<"states">>, state_orset}))))).
%
%
% lasp_peer_service:join(webserver_node@GrispAdhoc).
% net_adm:ping(webserver_node@GrispAdhoc).
%
% {ok, Set} = lasp:query({<<"states">>, state_orset}).
% lasp_peer_service:members().
% net_adm:ping(web_server_node_2@GrispAdhoc).
% {ok, Set} = lasp:query({<<"states">>, state_orset}), L = sets:to_list(Set), H = hd(L).
% lasp:update({<<"states">>, state_orset}, {rmv, H}, Self).

% {ok, Set2} = lasp:query(StateId).
% FilterFun = fun(Elem) -> case Elem of {Node, _S = #state{ luminosity = _Lum, sonar = _Sonar, gyro = _Gyro }} -> true; _ -> false end end.

% {ok, {FilteredId, _, _, _}} = lasp:declare({<<"filtered">>, state_orset}, state_orset).

% lasp:filter(StateId, FilterFun, StateId).

% lasp:filter(StateId, FilterFun, FilteredId).

% lasp:update(StateId, {rmv_all, })

% {ok, {NullId, _, _, _}} = lasp:declare({<<"nullset">>, state_orset}, state_orset).

% Tuple list :
% [{node@board,#state{luminosity = [{64,{shade,[],0}},
%                                   {243,{shade,[],0}},
%                                   {179,{shade,[],0}},
%                                   {115,{shade,[],0}},
%
%                                   {51,{shade,[],0}}],
%                     sonar = [1,2,3,4,5,6,7,8,9,10],
%                     gyro = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
%                             20]}}]

% Map Tuple list :
% [{node@board,#{luminosity => [{64,{shade,[],0}},
%                                   {243,{shade,[],0}},
%                                   {179,{shade,[],0}},
%                                   {115,{shade,[],0}},
%
%                                   {51,{shade,[],0}}],
%                     sonar => [1,2,3,4,5,6,7,8,9,10],
%                     gyro => [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
%                             20]}}]
% M = #{luminosity => ALSList, sonar => SonarList, gyro => GyroList}.


% Map list : WRONG
% [#{node@board => state}, #{node@board2 => state2}]
% MapFun = fun(Elem) ->
%   case Elem of
%     {node(), _OldState} ->
%       {node(), State};
%     _ ->
%       {node(), Elem}
%     end
%   end,

% lasp:map(SourceId, fun(X) -> X * 2 end, DestinationId).
% Node = node().
% MapFun = fun(Elem) -> case Elem of {node(), S = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }} -> {node(), State}; _ -> Elem end end.
% MapFun = fun(Elem) -> case Elem of {Node, S = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }} -> {Node, S#state{ luminosity = Lum, sonar = Sonar ++ [33], gyro = Gyro }}; _ -> Elem end end.
% MapFun2 = fun(Elem) -> case Elem of {node(), _OldState} -> {node(), a}; _ -> Elem end end.
% MapFun = fun(Elem) -> case Elem of {node(), S = #state{ luminosity = _, sonar = _, gyro = _ }} -> {node(), a}; _ -> Elem end end.
% MapFun3 = fun(Elem) -> case Elem of {node(), {state, _, _, _ }} -> {node(), a}; _ -> Elem end end.
% MapFun4 = fun(Elem) -> case {Elem} of {node(), a} -> {node(), b}; _ -> Elem end end.
% MapFun5 = fun(X) -> case X of 2 -> a; _ -> b end end.
% MapFun6 = fun(X) -> case X of {2} -> a; _ -> b end end.
% MapFun7 = fun(X) -> case X of {node(), 2} -> a; _ -> b end end.
% MapFun5 = fun(X) -> X rem 2 == 0 end
% lasp:map(StateId, MapFun, StateId).

% {ok, Set2} = lasp:query({BS, state_orset}).
% L = sets:to_list(Set).
% L = sets:to_list(Set).
% [Elem] = L.
% B = length(L).
% B.
% lasp:update({BS, state_orset}, {rmv, {node(), L}}, self()). ----> L = [{node@my_grisp_board, Data}]
% lasp:update({BS, state_orset}, {rmv, Elem}, self()). ----> Elem = {node@my_grisp_board, Data}
% lasp:update({BS, state_orset}, {add, {node(), [1,2,3,4]}}, self()).
% lasp:update({BS, state_orset}, {add, {node(), [1,2,3,4,5]}}, self()).

% ToFilter = lists:seq(1, 10, 1).
% Sub = [ X || X <- lists:seq(5, 10, 1) ].
% Filtered = lists:filter(fun(X) -> X rem 2 == 0 end, ToFilter).
% Filtered2 = lists:filter(fun(X) -> lists:member(X, Sub) end, ToFilter).


%%====================================================================
%% Lots of blah blah ahead
%%====================================================================
% Unable to perform MapReduce through Lasp functions
% Since a func s.t. :
%     map(Set, Fun) where
%     Fun :: fun(X) -> Elem
%     Elem :: member(Set)
%     X :: member(Set)
%  Therefore a Lasp call such as lasp:map(SrcId, Fun, DestId)
%  Does not perform the mapping from old to new state
%  If SrcId == DestId
% While adding values to the state s.t. each subsequent
% CRDT add-mutation is a superset containing all previous
% Elements implies full replication of every step
% And disallows aggregation
% Therefore the mapping is done locally and a
% Sequence of Lasp updates as :
%     > lasp:update(SrcId, {rmv, Subset}, self())
%  OR
%     > lasp:update(SrcId, {rmv_all, [Subset1, Subset2, Subset3 | ... ]}, self())
%  ok.
%     > lasp:update(SrcId, {add, Superset}, self())
%  ok.
%
% This method is the current closest to a Lasp variable aggregation



% FilterFun = fun (Elem) ->
% 		case Elem of
% 		  {Node,
% 		   _S = #state{luminosity = _Lum, sonar = _Sonar,
% 			       gyro = _Gyro}} ->
% 		      % {Node, S#state{ luminosity = Lum, sonar = Sonar ++ [33], gyro = Gyro }};
% 		      % {Node, State};
% 		      true;
% 		  _ -> false
% 		end
% 	end,
% MapFun = fun (Elem) ->
% 	     case Elem of
% 	       {Node,
% 		_S = #state{luminosity = _Lum, sonar = _Sonar,
% 			    gyro = _Gyro}} ->
% 		   % {Node, S#state{ luminosity = Lum, sonar = Sonar ++ [33], gyro = Gyro }};
% 		   {Node, State};
% 	       _ -> Elem
% 	     end
%      end,
