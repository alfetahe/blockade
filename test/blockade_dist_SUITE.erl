-module(blockade_dist_SUITE).

-define(NR_OF_NODES, 2).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([test_add_handler_dist/1]).

all() ->
    [{group, blockade_dist_group}].

groups() ->
    [{blockade_dist_group, [], [test_add_handler_dist]}].            

init_per_group(_GroupName, Config) ->
    Nodes =
    [?CT_PEER(["-pa", code:lib_dir(blockade) ++ "/ebin"])
     || _Nr <- lists:seq(1, ?NR_OF_NODES)],
    [unlink(Peer) || {_, Peer, _Node} <- Nodes], 
    [{nodes, Nodes} | Config].

end_per_group(_GroupName, Config) ->
    [peer:stop(Peer) || {_, Peer, _Node} <- ?config(nodes, Config)].

init_per_testcase(TestCase, Config) ->
    [erpc:call(Node,
               fun() ->
                  {ok, SupPid} = blockade_sup:start_link(#{name => TestCase}),
                  unlink(SupPid)
               end)
     || {_, _Peer, Node} <- ?config(nodes, Config)],
    Config. 

end_per_testcase(TestCase, Config) ->
    [rpc:call(Node, blockade_sup, stop, [TestCase]) || {_, _Peer, Node} <- ?config(nodes, Config)].

test_add_handler_dist(_Config) ->
    ok.
