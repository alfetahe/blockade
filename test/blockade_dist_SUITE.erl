-module(blockade_dist_SUITE).

-define(NR_OF_NODES, 1).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_add_handler_dist/1]).

all() ->
    [test_add_handler_dist].

init_per_testcase(TestCase, Config) ->
    Nodes =
        [?CT_PEER(["-pa", code:lib_dir(blockade) ++ "/ebin"])
         || _Nr <- lists:seq(1, ?NR_OF_NODES)],
    [erpc:call(Node,
               fun() ->
                  {ok, SupPid} = blockade_sup:start_link(#{name => TestCase}),
                  unlink(SupPid)
               end)
     || {_, _Peer, Node} <- Nodes],
    [{nodes, Nodes} | Config].

end_per_testcase(TestCase, Config) ->
    Nodes = ?config(nodes, Config),
    [rpc:call(Node, blockade_sup, stop, [TestCase]) || {_, _Peer, Node} <- Nodes],
    [peer:stop(Peer) || {_, Peer, _Node} <- Nodes],
    ok.

test_add_handler_dist(_Config) ->
    ok.
