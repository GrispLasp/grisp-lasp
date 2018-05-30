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
-export([start_link/0,
        start_link/1,
        get_data/0]).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-compile({nowarn_unused_function,
         [{stream_data, 2}]}).

-compile(export_all).

%%====================================================================
%% Macros
%%====================================================================

-define(PMOD_ALS_RANGE, lists:seq(1, 255, 1) ).
-define(PMOD_ALS_REFRESH_RATE, ?TEN ).
-define(PMOD_MAXSONAR_REFRESH_RATE, ?TEN ).
-define(PMOD_GYRO_REFRESH_RATE, ?TEN ).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    luminosity = [],
    sonar = [],
    gyro = []
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Mode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Mode}, []).

get_data() ->
      gen_server:call(?MODULE, {get_data}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init({Mode}) ->
  Shades = lists:duplicate(255, #shade{}),
  Range = ?PMOD_ALS_RANGE,

  List = lists:zipwith(fun
  (X,Y) ->
    {X, Y}
  end, Range, Shades),

  Dict = dict:from_list(List),

  State = #state{
    luminosity = Dict
  },
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
    _ ->
      {stop, unknown_launch_mode}
  end.

%%--------------------------------------------------------------------

handle_call({Request}, _From, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
    case Request of
      % guard clause for snippet
      get_data when is_atom(get_data) ->
        {reply, {ok, {Lum, Sonar, Gyro}}, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }};
      _ -> {reply, unknown_call, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(timeout, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
  Raw = pmod_als:raw(),
  RawSonar = pmod_maxsonar:get(),
  RawGyro = pmod_gyro:read_gyro(),

  NewLum = dict:update(Raw, fun(Shade) -> #shade{
    measurements = Shade#shade.measurements ++ [Raw],
    count = Shade#shade.count + 1} end, Lum),
  NewSonar = Sonar ++ [RawSonar],
  NewGyro = Gyro ++ [RawGyro],

  % ok = stream_data(?PMOD_ALS_REFRESH_RATE, als),
  % ok = stream_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar),
  % ok = stream_data(?PMOD_GYRO_REFRESH_RATE, gyro),
  ok = store_data(?PMOD_ALS_REFRESH_RATE, als, dict:to_list(NewLum), node(), self(), atom_to_binary(als, latin1)),
  ok = store_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar, NewSonar, node(), self(), atom_to_binary(maxsonar, latin1)),
  ok = store_data(?PMOD_GYRO_REFRESH_RATE, gyro, NewGyro, node(), self(), atom_to_binary(gyro, latin1)),
  {noreply, State#state{luminosity = NewLum, sonar = NewSonar, gyro = NewGyro}};

handle_info(als, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
  Raw = pmod_als:raw(),
  NewLum = dict:update(Raw, fun(Shade) -> #shade{
    measurements = Shade#shade.measurements ++ [Raw],
    count = Shade#shade.count + 1} end, Lum),
  % ok = stream_data(?PMOD_ALS_REFRESH_RATE, als),
  ok = store_data(?PMOD_ALS_REFRESH_RATE, als, dict:to_list(NewLum), node(), self(), atom_to_binary(gyro, latin1)),
  {noreply, State#state{luminosity = NewLum, sonar = Sonar, gyro = Gyro}};

handle_info(maxsonar, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
  RawSonar = pmod_maxsonar:get(),
  NewSonar = Sonar ++ [RawSonar],
  % ok = stream_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar),
  ok = store_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar, NewSonar, node(), self(), atom_to_binary(gyro, latin1)),
  {noreply, State#state{luminosity = Lum, sonar = NewSonar, gyro = Gyro}};

handle_info(gyro, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
  RawGyro = pmod_gyro:read_gyro(),
  NewGyro = Gyro ++ [RawGyro],
  % ok = stream_data(?PMOD_GYRO_REFRESH_RATE, gyro),
  ok = store_data(?PMOD_GYRO_REFRESH_RATE, gyro, NewGyro, node(), self(), atom_to_binary(gyro, latin1)),
  {noreply, State#state{luminosity = Lum, sonar = Sonar, gyro = NewGyro}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

stream_data(Rate, Sensor) ->
  erlang:send_after(Rate, self(), Sensor),
  ok.

%%--------------------------------------------------------------------

store_data(Rate, Type, SensorData, Node, Self, BitString) ->
    ?PAUSE10,
    lasp:update({BitString, state_orset}, {add, {Node, SensorData}}, Self),
    erlang:send_after(Rate, Self, Type),
    ok.

flood() ->
  List = lists:seq(1, 30000, 1),
  Fun = fun
    (Elem) when is_integer(Elem) ->
      ?PAUSE1,
      lasp:update({<<"flood">>, state_orset}, {add, {node(), Elem}}, self())
  end,
  ok = lists:foreach(Fun, List).
% lasp:query({<<"als">>, state_orset}).
% lasp:query({<<"sonar">>, state_orset}).
% lasp:query({<<"gyro">>, state_orset}).
