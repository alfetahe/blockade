-module(blockade_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("../include/blockade_header.hrl").

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([test_get_reset_opt/1, test_get_discard_opt/1, test_queue_prune/1,
         test_member_pids/1, test_rand_node/1, test_send_messages/1, test_dispatch_event/1,
         test_queue_event/1, test_dispatch_queued/1, test_startup_prio_confr/1,
         test_emit_priority/1, test_sync_priority/1]).

-define(NR_OF_NODES, 5).

all() ->
    [test_get_reset_opt,
     test_get_discard_opt,
     test_queue_prune,
     test_member_pids,
     test_rand_node,
     test_send_messages,
     test_dispatch_event,
     test_queue_event,
     test_dispatch_queued,
     test_startup_prio_confr,
     test_emit_priority,
     test_sync_priority].

init_per_suite(Config) ->
    Nodes =
        [?CT_PEER(["-pa", code:lib_dir(blockade) ++ "/ebin", "-connect_all", "false"])
         || _Nr <- lists:seq(1, ?NR_OF_NODES)],
    [unlink(Peer) || {_, Peer, _Node} <- Nodes],

    % Connect all peer nodes to each other.
    [erpc:call(Node,
               fun() -> [net_kernel:connect_node(PeerNode) || {_, _, PeerNode} <- Nodes] end)
     || {_, _Peer, Node} <- Nodes],

    [{nodes, Nodes} | Config].

end_per_suite(Config) ->
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

test_get_reset_opt(_Config) ->
    undefined = blockade_service:get_reset_opt(#manst{}, #{}),
    true = is_reference(blockade_service:get_reset_opt(#manst{}, #{reset_after => 1})),
    true =
        is_reference(blockade_service:get_reset_opt(#manst{schduler_ref = test},
                                                    #{reset_after => 2})),
    test = blockade_service:get_reset_opt(#manst{schduler_ref = test}, #{}).

test_get_discard_opt(_Config) ->
    ?DEFAULT_DISCARD_EVENTS = blockade_service:get_discard_opt(#{}, false),
    true = blockade_service:get_discard_opt(#{discard_events => true}, false),
    false = blockade_service:get_discard_opt(#{discard_events => false}, true),
    true = blockade_service:get_discard_opt(#{discard_events => true}, true),
    true = blockade_service:get_discard_opt(#{}, true).

test_queue_prune(_Config) ->
    Events =
        [{test, "data1", #{priority => 1}},
         {test, "data1_ex", #{priority => 1}},
         {test, "data2", #{priority => 2}},
         {test, "data3", #{priority => 3}},
         {test, "data3_ex", #{priority => 3}}],
    Events = blockade_service:queue_prune(Events, -15),
    [{test, "data2", #{priority := 2}},
     {test, "data3", #{priority := 3}},
     {test, "data3_ex", #{priority := 3}}] =
        blockade_service:queue_prune(Events, 2),
    [] = blockade_service:queue_prune(Events, 5),
    Events = blockade_service:queue_prune(Events, 0).

test_member_pids(Config) ->
    LocalPid = erlang:whereis(?PROCESS_NAME(test_member_pids, "helper")),
    Scope = ?PROCESS_NAME(test_member_pids, "pg"),
    [] = blockade_service:member_pids(Scope, test_event, local),
    blockade_test_helper:add_handler_nodes(test_member_pids,
                                           test_event,
                                           ?config(nodes, Config)),
    [LocalPid] = blockade_service:member_pids(Scope, test_event, local),
    GlobalMembers = blockade_service:member_pids(Scope, test_event, global),
    ?NR_OF_NODES + 1 = length(GlobalMembers),
    true = lists:all(fun(Pid) -> is_pid(Pid) end, GlobalMembers).

test_rand_node(_Config) ->
    true =
        lists:member(
            blockade_service:rand_node(), nodes()).

test_send_messages(Config) ->
    Pid = self(),
    Fun = fun() -> blockade_service:send_messages([Pid], test_event, test_payload) end,
    Fun(),
    [erpc:call(Node, Fun) || {_, _, Node} <- ?config(nodes, Config)],
    lists:all(fun(Msg) -> Msg =:= {test_event, test_payload} end,
              blockade_test_helper:all_messages([])),
    ok = blockade_service:send_messages([], test_event, test_payload).

test_dispatch_event(Config) ->
    Nodes = [node() | ?config(nodes, Config)],
    blockade_test_helper:start_pg_nodes(test_dispatch_event, Nodes),
    blockade_test_helper:add_handler_nodes(test_dispatch_event, test_event, Nodes),
    ExecFun =
        fun() ->
           blockade_service:dispatch_event(test_event,
                                           {test_dispatch_event, self()},
                                           test_dispatch_event,
                                           #{members => global}),
           receive
               test_dispatch_event ->
                   ok
           after 1000 ->
               false
           end
        end,
    Responses = [erpc:call(Node, ExecFun) || {_, _, Node} <- Nodes],
    true = lists:all(fun(Resp) -> Resp =:= ok end, Responses).

test_queue_event(_Config) ->
    E = test_event,
    P = test_payload,
    Eq = [{E, P, #{priority => 1, members => global}}],
    S1 = #manst{event_queue = Eq, discard_events = true, priority = 0},
    {event_discarded, S1} = blockade_service:queue_event(E, P, #{priority => -1}, S1),
    {event_queued, #manst{event_queue = [{E, P, #{priority := 0}} | Eq]}} =
        blockade_service:queue_event(E, P, #{priority => 0}, S1),
    {event_queued, #manst{event_queue = [{E, P, #{priority := 1}} | Eq]}} =
        blockade_service:queue_event(E, P, #{priority => 1}, S1),
    S2 = S1#manst{priority = -100},
    {event_discarded, S2} = blockade_service:queue_event(E, P, #{priority => -101}, S2),
    {event_queued, #manst{event_queue = [{E, P, #{priority := 100}} | Eq]}} =
        blockade_service:queue_event(E, P, #{priority => 100}, S2),
    {event_queued, #manst{event_queue = [{E, P, #{priority := -100}} | Eq]}} =
        blockade_service:queue_event(E, P, #{priority => -100}, S2),
    S3 = S2#manst{priority = 999, discard_events = false},
    {event_queued, #manst{event_queue = [{E, P, #{priority := -15}} | Eq]}} =
        blockade_service:queue_event(E, P, #{priority => -15}, S3),
    {event_queued, #manst{event_queue = [{E, P, #{priority := 15}} | Eq]}} =
        blockade_service:queue_event(E, P, #{priority => 15}, S3),
    {event_queued, #manst{event_queue = [{E, P, #{priority := 1000}} | Eq]}} =
        blockade_service:queue_event(E, P, #{priority => 1000}, S3).

test_dispatch_queued(Config) ->
    Nodes = [node() | ?config(nodes, Config)],
    blockade_test_helper:start_pg_nodes(test_dispatch_queued, Nodes),
    blockade_test_helper:add_handler_nodes(test_dispatch_queued, test_event, Nodes),
    Eq = [{test_event, test_payload, #{priority => 0, members => global}},
          {test_event, test_payload, #{priority => 1, members => global}},
          {test_event, test_payload, #{priority => 2, members => global}},
          {test_event, test_payload, #{priority => 3, members => global}}],
    Req1 = lists:reverse(Eq),
    Req1 = blockade_service:dispatch_queued([], any, any, Eq),
    Req2 = lists:sublist(Eq, 1),
    Req2 = blockade_service:dispatch_queued(Eq, test_dispatch_queued, 1, []),
    Req3 = lists:sublist(Eq, 2),
    Req3 = blockade_service:dispatch_queued(Eq, test_dispatch_queued, 2, []).

test_startup_prio_confr(_Config) ->
    false = blockade_service:startup_prio_confr(#{}),
    true = blockade_service:startup_prio_confr(#{priority => 0}).

test_emit_priority(_Config) ->
    blockade_service:emit_priority(test_emit_priority, 5000),
    blockade_service:emit_priority(test_emit_priority, 3000),
    RespFun = fun() -> gen_server:call(test_emit_priority, get_state) end,
    Responses = [erpc:call(Node, RespFun) || Node <- nodes()],
    true =
        lists:all(fun(#manst{emitted_priorites = Ep}) -> Ep =:= [3000, 5000] end, Responses).

test_sync_priority(_Config) ->
    999 = blockade_service:sync_priority([], 999),
    same = blockade_service:sync_priority([no, no, same, same], -1000),
    yes = blockade_service:sync_priority([no, yes], any),
    most = blockade_service:sync_priority([most, most, less], any).
