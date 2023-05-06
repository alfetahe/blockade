-module(blockade_service).

-export([rand_node/0]).

rand_node() ->
    Nodes = erlang:nodes(),
    lists:nth(
        rand:uniform(length(Nodes)), Nodes).
