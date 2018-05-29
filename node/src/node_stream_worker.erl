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
-export([start_link/0, get_data/0]).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

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

get_data() ->
      gen_server:call(?MODULE, {get_data}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([]) ->
  io:format("Starting stream worker ~n"),
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
  {ok, State, 10000}.

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
  ok = store_data(?PMOD_ALS_REFRESH_RATE, als, NewLum, node(), self(), erlang:atom_to_binary(als)),
  ok = store_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar, NewSonar, node(), self(), erlang:atom_to_binary(maxsonar)),
  ok = store_data(?PMOD_GYRO_REFRESH_RATE, gyro, NewGyro, node(), self(), erlang:atom_to_binary(gyro)),
  {noreply, State#state{luminosity = NewLum, sonar = NewSonar, gyro = NewGyro}};

handle_info(als, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
  Raw = pmod_als:raw(),
  NewLum = dict:update(Raw, fun(Shade) -> #shade{
    measurements = Shade#shade.measurements ++ [Raw],
    count = Shade#shade.count + 1} end, Lum),
  % ok = stream_data(?PMOD_ALS_REFRESH_RATE, als),
  ok = store_data(?PMOD_ALS_REFRESH_RATE, als, dict:to_list(NewLum), node(), self(), erlang:atom_to_binary(gyro)),
  {noreply, State#state{luminosity = NewLum, sonar = Sonar, gyro = Gyro}};

handle_info(maxsonar, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
  RawSonar = pmod_maxsonar:get(),
  NewSonar = Sonar ++ [RawSonar],
  % ok = stream_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar),
  ok = store_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar, NewSonar, node(), self(), erlang:atom_to_binary(gyro)),
  {noreply, State#state{luminosity = Lum, sonar = NewSonar, gyro = Gyro}};

handle_info(gyro, State = #state{ luminosity = Lum, sonar = Sonar, gyro = Gyro }) ->
  RawGyro = pmod_gyro:read_gyro(),
  NewGyro = Gyro ++ [RawGyro],
  % ok = stream_data(?PMOD_GYRO_REFRESH_RATE, gyro),
  ok = store_data(?PMOD_GYRO_REFRESH_RATE, gyro, NewGyro, node(), self(), erlang:atom_to_binary(gyro)),
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
    lasp:update({BitString, state_orset}, {add, {Node, SensorData}}, Self),
    erlang:send_after(Rate, Self, Type).
