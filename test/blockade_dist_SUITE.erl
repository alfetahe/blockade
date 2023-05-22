-module(blockade_dist_SUITE).

-define(NR_OF_NODES, 2).

-include_lib("common_test/include/ct.hrl").

-include("../include/blockade_header.hrl").

-behaviour(ct_suite).

-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2, test_get_handlers_dist/1, test_get_events_dist/1]).
-export([test_add_handler_dist/1, test_remove_handler_dist/1, test_dispatch_sync_dist/1,
         test_dispatch_dist/1, test_dispatch_dist_prio/1, test_dispatch_dist_memb_local/1,
         test_dispatch_dist_memb_global/1, test_get_set_priority_dist/1,
         test_get_event_queue_dist/1, test_prune_event_queue_dist/1]).

all() ->
    [{group, blockade_dist_group}].

groups() ->
    [{blockade_dist_group,
      [],
      [test_add_handler_dist,
       test_remove_handler_dist,
       test_get_handlers_dist,
       test_get_events_dist,
       test_dispatch_sync_dist,
       test_dispatch_dist,
       test_dispatch_dist_prio,
       test_dispatch_dist_memb_local,
       test_dispatch_dist_memb_global,
       test_get_set_priority_dist,
       test_get_event_queue_dist,
       test_prune_event_queue_dist]}].

init_per_group(_GroupName, Config) ->
    Nodes =
        [?CT_PEER(["-pa", code:lib_dir(blockade) ++ "/ebin", "-connect_all", "false"])
         || _Nr <- lists:seq(1, ?NR_OF_NODES)],
    [unlink(Peer) || {_, Peer, _Node} <- Nodes],

    % Connect all peer nodes to each other.
    [erpc:call(Node,
               fun() -> [net_kernel:connect_node(PeerNode) || {_, _, PeerNode} <- Nodes] end)
     || {_, _Peer, Node} <- Nodes],

    [{nodes, Nodes} | Config].

end_per_group(_GroupName, Config) ->
    [peer:stop(Peer) || {_, Peer, _Node} <- ?config(nodes, Config)].

init_per_testcase(TestCase, Config) ->
    Nodes = ?config(nodes, Config),
    StartHelperFun =
        fun() ->
           {ok, TestWorkerPid} = blockade_test_helper:start_link(TestCase),
           unlink(TestWorkerPid)
        end,
    [erpc:call(Node, fun() -> StartHelperFun() end) || {_, _Peer, Node} <- Nodes],
    StartHelperFun(),

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
    Nodes = ?config(nodes, Config),
    StopHelperFun = fun() -> blockade_test_helper:stop(TestCase) end,
    [erpc:call(Node, StopHelperFun) || {_, _Peer, Node} <- Nodes],
    StopHelperFun(),
    blockade_sup:stop(TestCase),
    [rpc:call(Node, blockade_sup, stop, [TestCase])
     || {_, _Peer, Node} <- ?config(nodes, Config)].

test_add_handler_dist(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_add_handler_dist, test_event, Nodes),
    Pids =
        [self()] ++ blockade_test_helper:get_pids(test_add_handler_dist, ?config(nodes, Config)),
    HandlerPids = pg:get_members(test_add_handler_dist, test_event),
    lists:all(fun(Pid) -> lists:member(Pid, Pids) end, HandlerPids).

test_remove_handler_dist(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_remove_handler_dist, test_event, Nodes),
    Pids =
        [self()] ++ blockade_test_helper:get_pids(test_add_handler_dist, ?config(nodes, Config)),
    HandlerPids = pg:get_members(test_remove_handler_dist, test_event),
    true = lists:all(fun(Pid) -> lists:member(Pid, Pids) end, HandlerPids),
    blockade_test_helper:remove_handler_nodes(test_remove_handler_dist, test_event, Nodes),
    [] = pg:get_members(test_remove_handler_dist, test_event).

test_get_handlers_dist(Config) ->
    Nodes = [{any, any, node()} | ?config(nodes, Config)],
    GetHandlersFun = fun() -> blockade:get_handlers(test_get_handlers_dist, test_event) end,
    Res = [erpc:call(Node, GetHandlersFun) || {_, _, Node} <- Nodes],
    true = lists:all(fun(Resp) -> Resp =:= {ok, []} end, Res),
    blockade_test_helper:add_handler_nodes(test_get_handlers_dist,
                                           test_event,
                                           ?config(nodes, Config)),
    blockade_test_helper:test_sync_msg(test_get_handlers_dist, ?config(nodes, Config)),
    NewRes = [erpc:call(Node, GetHandlersFun) || {_, _, Node} <- Nodes],
    Members = pg:get_members(test_get_handlers_dist_pg, test_event),
    true =
        lists:all(fun({ok, Handlers}) -> lists:sort(Handlers) =:= lists:sort(Members) end,
                  NewRes).

test_get_events_dist(Config) ->
    Nodes = [{any, any, node()} | ?config(nodes, Config)],
    AddEventFun =
        fun() ->
           blockade:add_handler(test_get_events_dist,
                                list_to_atom(atom_to_list(test_get_events_dist)
                                             ++ "_"
                                             ++ atom_to_list(node())))
        end,
    Resp1 = [erpc:call(Node, AddEventFun) || {_, _, Node} <- Nodes],
    true = lists:all(fun(Res) -> Res =:= ok end, Resp1),
    timer:sleep(100), % Pg has not propagated the results yet.
    GetEventsFun = fun() -> blockade:get_events(test_get_events_dist) end,
    Resp2 = [erpc:call(Node, GetEventsFun) || {_, _, Node} <- Nodes],
    Groups = pg:which_groups(test_get_events_dist_pg),
    true = lists:all(fun({ok, Events}) -> Events =:= Groups end, Resp2).

test_dispatch_sync_dist(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_dispatch_sync_dist, test_event, Nodes),
    blockade:dispatch_sync(test_dispatch_sync_dist,
                           test_event,
                           {test_dispatch_sync_dist_msg, self()}),
    % Need to do one test sync call to make sure all nodes have handled the event.
    blockade_test_helper:test_sync_msg(test_dispatch_sync_dist, Nodes),
    AllMessages = blockade_test_helper:get_all_messages([]),
    true = lists:all(fun(Resp) -> Resp =:= test_dispatch_sync_dist_msg end, AllMessages).

test_dispatch_dist(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_dispatch_dist, test_event, Nodes),
    blockade:dispatch(test_dispatch_dist, test_event, {test_dispatch_dist_msg, self()}),
    % Need to do one test sync call to make sure all nodes have handled the event.
    blockade_test_helper:test_sync_msg(test_dispatch_dist, Nodes),
    AllMessages = blockade_test_helper:get_all_messages([]),
    true = lists:all(fun(Resp) -> Resp =:= test_dispatch_dist_msg end, AllMessages).

test_dispatch_dist_prio(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_dispatch_dist_prio, prio_1, Nodes),
    blockade_test_helper:add_handler_nodes(test_dispatch_dist_prio, prio_0, Nodes),
    blockade_test_helper:add_handler_nodes(test_dispatch_dist_prio, prio_minus_1, Nodes),
    blockade_test_helper:add_handler_nodes(test_dispatch_dist_prio, prio_2, Nodes),
    blockade:set_priority(test_dispatch_dist_prio, 1),
    blockade:dispatch(test_dispatch_dist_prio, prio_0, {prio_0, self()}),
    blockade:dispatch(test_dispatch_dist_prio,
                      prio_minus_1,
                      {prio_minus_1, self()},
                      #{priority => -1}),
    blockade:dispatch(test_dispatch_dist_prio, prio_1, {prio_1, self()}, #{priority => 1}),
    blockade:dispatch(test_dispatch_dist_prio, prio_2, {prio_2, self()}, #{priority => 2}),
    % Need to do one test sync call to make sure all nodes have handled the event.
    blockade_test_helper:test_sync_msg(test_dispatch_dist_prio, Nodes),
    AllMessages = blockade_test_helper:get_all_messages([]),
    true = lists:all(fun(Resp) -> Resp =:= prio_1 orelse Resp =:= prio_2 end, AllMessages).

test_dispatch_dist_memb_local(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_dispatch_dist_memb_local, memb_local, Nodes),
    blockade:dispatch(test_dispatch_dist_memb_local,
                      memb_local,
                      {memb_local, self()},
                      #{members => local}),
    % Need to do one test sync call to make sure all nodes have handled the event.
    blockade_test_helper:test_sync_msg(test_dispatch_dist_memb_local, Nodes),
    AllMessages = blockade_test_helper:get_all_messages([]),
    1 = length(AllMessages),
    true = lists:all(fun(Resp) -> Resp =:= memb_local end, AllMessages).

test_dispatch_dist_memb_global(Config) ->
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(test_dispatch_dist_memb_global,
                                           memb_global,
                                           Nodes),
    blockade:dispatch(test_dispatch_dist_memb_global,
                      memb_global,
                      {memb_global, self()},
                      #{members => global}),
    % Need to do one test sync call to make sure all nodes have handled the event.
    blockade_test_helper:test_sync_msg(test_dispatch_dist_memb_global, Nodes),
    AllMessages = blockade_test_helper:get_all_messages([]),
    Total = ?NR_OF_NODES + 1,
    Total = length(AllMessages),
    true = lists:all(fun(Resp) -> Resp =:= memb_global end, AllMessages).

test_get_set_priority_dist(Config) ->
    Nodes = ?config(nodes, Config),
    blockade:set_priority(test_get_set_priority_dist, 5000),
    Prios1 = blockade_test_helper:get_priorities(test_get_set_priority_dist, Nodes),
    true = lists:all(fun(Prio) -> Prio =:= {ok, 5000} end, Prios1),
    blockade:set_priority(test_get_set_priority_dist, 9),
    Prios2 = blockade_test_helper:get_priorities(test_get_set_priority_dist, Nodes),
    true = lists:all(fun(Prio) -> Prio =:= {ok, 9} end, Prios2).

test_get_event_queue_dist(Config) ->
    E = test_get_event_queue_dist,
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(E, test_event, Nodes),
    blockade:set_priority(E, 2, #{discard_events => true}),
    blockade:dispatch(E, test_event, {test_event, self()}, #{priority => 1}),
    blockade_test_helper:test_sync_msg(E, Nodes),
    AllQueues1 = blockade_test_helper:get_event_queues(E, Nodes),
    true = lists:all(fun(Queue) -> Queue =:= {ok, []} end, AllQueues1),
    blockade:set_priority(E, 2, #{discard_events => false}),
    blockade:dispatch(E, test_event, {test_event, self()}, #{priority => 1}),
    blockade:dispatch(E, test_event, {test_event, self()}, #{priority => -1}),
    blockade_test_helper:test_sync_msg(E, Nodes),
    [First2 | AllQueues2] = blockade_test_helper:get_event_queues(E, Nodes),
    true = lists:all(fun(Queue) -> Queue =:= {ok, []} end, AllQueues2),
    First2 =
        {ok,
         [{test_event, {test_event, self()}, #{priority => 1}},
          {test_event, {test_event, self()}, #{priority => -1}}]},
    blockade:set_priority(E, 2),
    blockade:dispatch(E, test_event, {test_event, self()}, #{priority => 0}),
    blockade_test_helper:test_sync_msg(E, Nodes),
    [First3 | AllQueues3] = blockade_test_helper:get_event_queues(E, Nodes),
    true = lists:all(fun(Queue) -> Queue =:= {ok, []} end, AllQueues3),
    First3 =
        {ok,
         [{test_event, {test_event, self()}, #{priority => -1}},
          {test_event, {test_event, self()}, #{priority => 1}},
          {test_event, {test_event, self()}, #{priority => 0}}]}.

test_prune_event_queue_dist(Config) ->
    E = test_prune_event_queue_dist,
    Nodes = ?config(nodes, Config),
    blockade_test_helper:add_handler_nodes(E, test_event, Nodes),
    blockade:set_priority(E, -5, #{discard_events => false}),
    blockade:dispatch(E, test_event, {test_event, self()}, #{priority => -15}),
    blockade:dispatch(E, test_event, {test_event, self()}, #{priority => -10}),
    blockade_test_helper:test_sync_msg(E, Nodes),
    blockade:prune_event_queue(E),
    AllQueues1 = blockade_test_helper:get_event_queues(E, Nodes),
    true = lists:all(fun(Queue) -> Queue =:= {ok, []} end, AllQueues1).
