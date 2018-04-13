-module(node_discovery).
-compile(export_all).

send() ->
  {ok, S} =  gen_udp:open(5010, [{broadcast, true}]),
  Node = atom_to_list(node()),
  io:format("=== Broadcasting Node ~p === ~n", [Node]),
  gen_udp:send(S, {169,254,255,255}, 6000, Node),
  gen_udp:close(S).


local_ip_v4() ->
  {ok, Addrs} = inet:getifaddrs(),
  hd([
  Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
  size(Addr) == 4, Addr =/= {127,0,0,1}
  ]).

listen() ->
  io:format("Listening for broadcasts~n"),
  spawn_link(
  fun () ->
    {ok, _} = gen_udp:open(6000),
    loop()
  end).


loop() ->
  receive
    {_, _, From, _, NodeNameList} ->
      NodeName = list_to_atom(NodeNameList),
      io:format("=== Received ~p from ~p ===~n", [NodeName, From]),
      LocalIPV4 = local_ip_v4(),
      io:format("=== My Ip is ~p ===~n", [LocalIPV4]),
      NodeExists = lists:member(NodeName, nodes()),
      case NodeName of
        _ when From =/= LocalIPV4, NodeExists =:= true ->
          io:format("=== ~p exists in nodes() ===~n", [NodeName]);
        _ when From =/= LocalIPV4, NodeExists =/= true ->
          io:format("=== ~p does not exist in nodes() ===~n", [NodeName]),
          Ping = net_adm:ping(NodeName),
          case Ping of
            pong ->
              io:format("Pinged ~p ~n", [NodeName]);
            pang ->
              io:format("Could not ping ~p ~n", [NodeName])
          end;
        _ when From =:= LocalIPV4 ->
          io:format("=== Received broadcast from self ===~n");
        _ ->
          io:format("Error -")
      end;
    _ ->
      io:format("Error /")
    end,
  loop().
