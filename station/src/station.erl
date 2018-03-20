% @doc robot public API.
% @end
-module(station).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([getn/0]).
-export([getip/0]).

%% API
-export([get_local_ip_from_subnet/1, get_ip_from_subnet/2,
cidr_network/1, cidr_netmask/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    {ok, Supervisor} = station_sup:start_link(),

    % application:load(partisan),
    % ok = partisan_config:init(),
    % partisan_config:set(partisan_peer_service_manager, partisan_hyparview_peer_service_manager),
    % partisan_sup:start_link(),
    % lasp_sup:start_link(),

    % partisan_config:set(partisan_peer_service_manager, partisan_hyparview_peer_service_manager),
    % partisan_config:set(peer_ip, {192,168,1,11}),
    % partisan_hyparview_peer_service_manager:start_link(),
    % partisan_default_peer_service_manager:start_link(),
    % grisp_led:pattern(1, [{100, Random}]),
    % application:load(partisan),
    % partisan_sup:start_link(),
    % partisan_default_peer_service_manager:start_link(),
    % partisan_default_peer_service_manager:myself(),
    % partisan_peer_service_manager:myself(),
    % partisan_hyparview_peer_service_manager:myself().
    % partisan_peer_service:members().
    % partisan_peer_service:join(lasp1@dan).
    % lasp_peer_service:join(#{name => 'lasp1@dan', listen_addrs => [#{ip => {192,168,1,3}, port => 4369}]}).
    % -internal_epmd epmd_sup
    % net_adm:ping(lasp1@dan).
    % Epmd = (net_kernel:epmd_module()),
    % {ok, Subnets} = inet:getif(),
    {ok, Supervisor}.

stop(_State) -> ok.

getip() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).

getn() ->
  Epmd = (net_kernel:epmd_module()),
  {ok, Subnets} = inet:getif(),
  % SubnetsList = tuple_to_list(Subnets),
  % SubnetsList.
  SubnetsNestedList = lists:map(fun
    (Tuple) ->
      List = tuple_to_list(Tuple),
      Filtered = lists:filtermap(fun
        (Elem) ->
          {true, Elem}
      end, List)
  end, Subnets),
  % SubnetsNestedList.
  OwnAddrs = lists:map(fun
    (Set) ->
      Own = hd(Set),
      % inet:gethostbyaddr(Own)
      Sub = cidr_network({Own, 24})
  end, SubnetsNestedList),
  OwnAddrs.

% pingall({I1, I2, I3, I4}) ->
%
%% @type ipv4() = {integer(), integer(), integer(), integer()}


%%--------------------------------------------------------------------
%% @spec get_local_ip_from_subnet({Network :: ipv4(), Netmask :: ipv4()}) ->
%%         {ok, ipv4()} | undefined | {error, Reason, Data}
%% @doc  Returns an IP address that is assigned to a local network interface and
%%       belongs to the specified subnet.
%%--------------------------------------------------------------------
get_local_ip_from_subnet({{_I1, _I2, _I3, _I4}, {_N1, _N2, _N3, _N4}} = Subnet) ->
    case inet:getif() of
        {ok, AddrList} when is_list(AddrList) ->
            get_ip_from_subnet(Subnet, [Ip || {Ip, _Broadcast, _Netmask} <- AddrList]);
        InvalidAddrList ->
            {error, invalid_network_interface, [InvalidAddrList]}
    end;
get_local_ip_from_subnet({{_I1, _I2, _I3, _I4} = Ip, Bits})
  when is_integer(Bits), Bits >= 0, Bits =< 32 ->
    get_local_ip_from_subnet({Ip, cidr_netmask(Bits)}).



%%--------------------------------------------------------------------
%% @spec get_ip_from_subnet({Network :: ipv4(), Netmask :: ipv4(), [ipv4()]}) ->
%%         {ok, ipv4()} | undefined | {error, Reason, Data}
%% @doc  Returns the first IP address in the list received as argument that
%%       belongs to the specified subnet.
%%--------------------------------------------------------------------
get_ip_from_subnet({{I1, I2, I3, I4}, {N1, N2, N3, N4} = Netmask}, AddrList) ->
    get_ip_from_normalized_subnet({{I1 band N1, I2 band N2, I3 band N3, I4 band N4},
                                   Netmask}, AddrList);
get_ip_from_subnet({{_I1, _I2, _I3, _I4} = Ip, Bits}, AddrList)
  when is_integer(Bits), Bits >= 0, Bits =< 32 ->
    get_ip_from_subnet({Ip, cidr_netmask(Bits)}, AddrList).


get_ip_from_normalized_subnet({{I1, I2, I3, I4}, {N1, N2, N3, N4}} = Subnet,
                              [{A1, A2, A3, A4} = Addr | Tail]) ->
    if ((A1 band N1) =:= I1 andalso
        (A2 band N2) =:= I2 andalso
        (A3 band N3) =:= I3 andalso
        (A4 band N4) =:= I4) ->
            {ok, Addr};
       true ->
            get_ip_from_normalized_subnet(Subnet, Tail)
    end;
get_ip_from_normalized_subnet(_Subnet, []) ->
    undefined.


%%--------------------------------------------------------------------
%% @spec cidr_network({Addr :: ipv4(), Bits :: integer()}) -> ipv4()
%% @doc  Return the subnet corresponding the the IP address and network bits
%%       received in CIDR format.
%%--------------------------------------------------------------------
cidr_network({{I1, I2, I3, I4}, Bits}) when is_integer(Bits) andalso Bits =< 32 ->
    ZeroBits = 8 - (Bits rem 8),
    Last = (16#ff bsr ZeroBits) bsl ZeroBits,

    case (Bits div 8) of
        0 ->
            {(I1 band Last), 0, 0, 0};
        1 ->
            {I1, (I2 band Last), 0, 0};
        2 ->
            {I1, I2, (I3 band Last), 0};
        3 ->
            {I1, I2, I3, (I4 band Last)};
        4 ->
            {I1, I2, I3, I4}
    end.


%%--------------------------------------------------------------------
%% @spec cidr_netmask(Bits :: integer()) -> ipv4()
%% @doc  Return the netmask corresponding to the network bits received in CIDR
%%       format.
%%--------------------------------------------------------------------
cidr_netmask(Bits) when is_integer(Bits) andalso Bits =< 32 ->
    ZeroBits = 8 - (Bits rem 8),
    Last = (16#ff bsr ZeroBits) bsl ZeroBits,

    case (Bits div 8) of
        0 ->
            {(255 band Last), 0, 0, 0};
        1 ->
            {255, (255 band Last), 0, 0};
        2 ->
            {255, 255, (255 band Last), 0};
        3 ->
            {255, 255, 255, (255 band Last)};
        4 ->
            {255, 255, 255, 255}
    end.
  % Foldfun = fun
  %   (Netmasks, Acc) ->
  %   Netmasks
  % end,
  % Nestedfun = fun
  %   (Address) ->
  %     % Epmd:names(Address)
  %     % {cool, tuple_to_list(Address)}
  %     AddressList = tuple_to_list(Address),
  %     lists:map(fun
  %       (Addr) ->
  %         % net_adm:ping(Addr),
  %         % {ok, Hostent} = inet:gethostbyaddr(Addr),
  %         % {cool, Addr, Hostent}
  %         {Code, Host} = inet:gethostbyaddr(Addr),
  %         case Host of
  %           {error, nxdomain} ->
  %               % io:format("err");
  %               {uncool, error};
  %           _Else ->
  %               % {hostent,Hostname,L,inet,Inet,AddrList} = Host,
  %               Host
  %         end
  %         % inet:gethostbyaddr(Addr)
  %     end, AddressList)
  %   end,
  % Mapfun = fun
  %   (Subnet) ->
  %     Hosts = Nestedfun(Subnet),
  %     Hosts
  % end,
  % % Res = lists:foldl(Foldfun, [], Subnets),
  % Res = lists:map(Mapfun, Subnets),
  % Result = lists:map(fun
  %   (Array) ->
  %     Array
  % end, Res),
  % {ok, Result}.
  % ok.
