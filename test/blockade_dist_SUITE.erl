-module(blockade_dist_SUITE).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_add_handler_dist/1]).

all() ->
    [test_add_handler_dist].

init_per_testcase(_TestCase, Config) ->
    {ok, Peer, Node} = {ok, 1, 2},
    [{peer_node, {Peer, Node}} | Config].

end_per_testcase(_TestCase, Config) ->
    {Peer, _Node} = ?config(peer_node, Config),
    peer:stop(Peer).

test_add_handler_dist(_Config) ->
    ok.
