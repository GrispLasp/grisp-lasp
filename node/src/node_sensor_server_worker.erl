-module(node_sensor_server_worker).
-author('Alex Carlier').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, creates/1, read/1, terminate/1]).

% These are all wrappers for calls to the server
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
creates(Sensor_type) -> gen_server:call(?MODULE, {creates, Sensor_type}).
read(Sensor_type) -> gen_server:call(?MODULE, {read, Sensor_type}).
terminate(Sensor_type) -> gen_server:call(?MODULE, {terminate, Sensor_type}).

% This is called when a connection is made to the server
init([]) ->
  SensorList = [],
  io:format("Gen server for sensors has been initialized ~n"),
  {ok, SensorList}.

% handle_call is invoked in response to gen_server:call
handle_call({creates, Sensor_type}, _From, SensorList) ->
  Response = case lists:member(Sensor_type,SensorList) of
    true ->
      NewSensorList = SensorList,
      {already_created, Sensor_type};
    false ->
      % create a sensor of type Sensor_type
      NewSensorList = lists:append([Sensor_type], SensorList),
      case Sensor_type of
        pmod_maxsonar ->
          % {ok, Supervisor} = pmod_maxsonar:start_link(uart)
          application:set_env(grisp, devices, [{uart, pmod_maxsonar}]),
          {ok, _} = pmod_maxsonar:start_link(uart);
        pmod_acl2 ->
          application:set_env(grisp, devices, [{spi1, pmod_acl2}]),
          spawn_link(fun() -> process() end)
      end,
      ok
  end,
  {reply, Response, NewSensorList};



handle_call({read, Sensor_type}, _From, SensorList) ->
  Response = case lists:member(Sensor_type,SensorList) of
    true -> ReadValue = readSensor(Sensor_type),
      {read, ReadValue};
    false ->
      {sensor_not_created, Sensor_type}
  end,
  {reply, Response, SensorList};

handle_call({terminate, Sensor_type}, _From, SensorList) ->
  NewLibrary = lists:delete(Sensor_type, SensorList),
  {reply, ok, NewLibrary};

handle_call(_Message, _From, SensorList) ->
  {reply, error, SensorList}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, SensorList) -> {noreply, SensorList}.
handle_info(_Message, SensorList) -> {noreply, SensorList}.
terminate(_Reason, _SensorList) -> ok.
code_change(_OldVersion, SensorList, _Extra) -> {ok, SensorList}.

readSensor(Sensor_type) ->
  case Sensor_type of
    temp -> math:floor(rand:uniform()*30);
    press -> rand:uniform();
    pmod_maxsonar -> pmod_maxsonar:get();
    _ -> {this_type_of_sensor_was_not_created}
  end.


process() ->
    {X, Y, Z} = pmod_acl2:g(),
    Color = {color(X), color(Y), color(Z)},
    grisp_led:color(1, Color),
    grisp_led:color(2, Color),
    timer:sleep(10),
    process().

color(Val) -> trunc((abs(Val) / 2.0) + 0.8) rem 2.
