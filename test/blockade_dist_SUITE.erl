-module(blockade_dist_SUITE).

-define(NR_OF_NODES, 2).

-include_lib("common_test/include/ct.hrl").

-include("../include/blockade_header.hrl").

-behaviour(ct_suite).

-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
-export([test_add_handler_dist/1, test_remove_handler/1]).

all() ->
    [{group, blockade_dist_group}].

groups() ->
    [{blockade_dist_group, [], [test_add_handler_dist, test_remove_handler]}].

init_per_group(_GroupName, Config) ->
    Nodes =
        [?CT_PEER(["-pa", code:lib_dir(blockade) ++ "/ebin"])
         || _Nr <- lists:seq(1, ?NR_OF_NODES)],
    [unlink(Peer) || {_, Peer, _Node} <- Nodes],

    % Connect all peer nodes to each other and start test worker.
    [erpc:call(Node,
               fun() ->
                  {ok, TestWorkerPid} = blockade_test_helper:start_link([]),
                  unlink(TestWorkerPid),
                  [net_kernel:connect_node(PeerNode) || {_, _, PeerNode} <- Nodes]
               end)
     || {_, _Peer, Node} <- Nodes],

    [{nodes, Nodes} | Config].

end_per_group(_GroupName, Config) ->
    [peer:stop(Peer) || {_, Peer, _Node} <- ?config(nodes, Config)].

init_per_testcase(TestCase, Config) ->
    blockade_sup:start_link(TestCase, #{priority => ?DEFAULT_PRIORITY}),
    [erpc:call(Node,
               fun() ->
                  {ok, SupPid} =
                      blockade_sup:start_link(TestCase, #{priority => ?DEFAULT_PRIORITY}),
                  unlink(SupPid)
               end)
     || {_, _Peer, Node} <- ?config(nodes, Config)],
    Config.

end_per_testcase(TestCase, Config) ->
    blockade_sup:stop(TestCase),
    [rpc:call(Node, blockade_sup, stop, [TestCase])
     || {_, _Peer, Node} <- ?config(nodes, Config)].

test_add_handler_dist(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_add_handler_dist, test_event, Nodes),
    Pids = [self()] ++ blockade_test_helper:get_pids(?config(nodes, Config)),
    HandlerPids = pg:get_members(test_add_handler_dist, test_event),
    lists:all(fun(Pid) -> lists:member(Pid, Pids) end, HandlerPids).

test_remove_handler(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_remove_handler, test_event, Nodes),
    Pids = [self()] ++ blockade_test_helper:get_pids(?config(nodes, Config)),
    HandlerPids = pg:get_members(test_remove_handler, test_event),
    lists:all(fun(Pid) -> lists:member(Pid, Pids) end, HandlerPids),
    blockade_test_helper:remove_handler_nodes(test_remove_handler, test_event, Nodes),
    [] = pg:get_members(test_remove_handler, test_event).
