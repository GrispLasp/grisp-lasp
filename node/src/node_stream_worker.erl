%%%-------------------------------------------------------------------
%% @author Igor Kopestenski <i.kopest@gmail.com>
%%   [https://github.com/Laymer/GrispLasp]
%% @doc This is a <em>simulation sensor data stream</em> module.
%% @end
%%%-------------------------------------------------------------------

-module(node_stream_worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

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
-define(PMOD_ALS_REFRESH_RATE, 3000 ).
-define(PMOD_MAXSONAR_REFRESH_RATE, 5000 ).

%%====================================================================
%% Records
%%====================================================================

-record(shade, {
    measurements = [],
    count = 0,
    avg = 0
}).

-record(state, {
    luminosity = [],
    sonar = []
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([]) ->
  io:format("Starting ALS and MAXSonar sensors ~n"),
  application:set_env(grisp, devices, [{spi2, pmod_als}]),
  application:set_env(grisp, devices, [{uart, pmod_maxsonar}]),
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
  grisp_devices:setup(),
  {ok, State, 10000}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(timeout, State = #state{ luminosity = Lum, sonar = Sonar }) ->
  Raw = pmod_als:raw(),
  RawSonar = pmod_maxsonar:get(),

  io:format("Raw = ~p ~n", [Raw]),
  io:format("Raw Sonar = ~p ~n", [RawSonar]),

  NewLum = dict:update(Raw, fun(Shade) -> #shade{
    measurements = Shade#shade.measurements ++ [Raw],
    count = Shade#shade.count + 1} end, Lum),
  NewSonar = Sonar ++ [RawSonar],

  ok = stream_data(?PMOD_ALS_REFRESH_RATE, als),
  ok = stream_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar),

  {noreply, State#state{luminosity = NewLum, sonar = NewSonar}};

handle_info(als, State = #state{ luminosity = Lum, sonar = Sonar }) ->
  Raw = pmod_als:raw(),
  io:format("Raw = ~p ~n", [Raw]),
  NewLum = dict:update(Raw, fun(Shade) -> #shade{
    measurements = Shade#shade.measurements ++ [Raw],
    count = Shade#shade.count + 1} end, Lum),
  ok = stream_data(?PMOD_ALS_REFRESH_RATE, als),
  {noreply, State#state{luminosity = NewLum, sonar = Sonar}};

handle_info(maxsonar, State = #state{ luminosity = Lum, sonar = Sonar }) ->
  RawSonar = pmod_maxsonar:get(),
  io:format("Raw Sonar = ~p ~n", [RawSonar]),
  NewSonar = Sonar ++ [RawSonar],
  ok = stream_data(?PMOD_MAXSONAR_REFRESH_RATE, maxsonar),
  {noreply, State#state{luminosity = Lum, sonar = NewSonar}};

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
