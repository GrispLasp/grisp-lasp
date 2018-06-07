-module(node_config).

-export([get/1]).

get(Key) ->
    {ok, Value} = application:get_env(node, Key), Value.
