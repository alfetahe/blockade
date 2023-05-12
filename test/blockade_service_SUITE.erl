-module(blockade_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("../include/blockade_header.hrl").

-behaviour(ct_suite).

-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).
-export([test_get_reset_opt/1, test_get_discard_opt/1, test_queue_prune/1,
         test_member_pids/1]).

-define(NR_OF_NODES, 3).

all() ->
    [{group, blockade_service_group}].

groups() ->
    [{blockade_service_group,
      [],
      [test_get_reset_opt, test_get_discard_opt, test_queue_prune, test_member_pids]}].

init_per_group(_GroupName, Config) ->
    Nodes =
        [?CT_PEER(["-pa", code:lib_dir(blockade) ++ "/ebin"])
         || _Nr <- lists:seq(1, ?NR_OF_NODES)],
    [unlink(Peer) || {_, Peer, _Node} <- Nodes],

    % Connect all peer nodes to each other and start test worker.
    [erpc:call(Node,
               fun() ->
                  {ok, TestWorkerPid} = blockade_test_worker:start_link([]),
                  unlink(TestWorkerPid),
                  [net_kernel:connect_node(PeerNode) || {_, _, PeerNode} <- Nodes]
               end)
     || {_, _Peer, Node} <- Nodes],

    [{nodes, Nodes} | Config].

end_per_group(_GroupName, Config) ->
    [peer:stop(Peer) || {_, Peer, _Node} <- ?config(nodes, Config)].

init_per_testcase(TestCase, Config) ->
    blockade_sup:start_link(#{name => TestCase}),

    [erpc:call(Node,
               fun() ->
                  {ok, SupPid} = blockade_sup:start_link(#{name => TestCase}),
                  unlink(SupPid)
               end)
     || {_, _Peer, Node} <- ?config(nodes, Config)],
    Config.

end_per_testcase(TestCase, Config) ->
    blockade_sup:stop(TestCase),
    [rpc:call(Node, blockade_sup, stop, [TestCase])
     || {_, _Peer, Node} <- ?config(nodes, Config)].

test_get_reset_opt(_Config) ->
    undefined = blockade_service:get_reset_opt(#manrec{}, #{}),
    true = is_reference(blockade_service:get_reset_opt(#manrec{}, #{reset_after => 1})),
    true =
        is_reference(blockade_service:get_reset_opt(#manrec{schduler_ref = test},
                                                    #{reset_after => 2})),
    test = blockade_service:get_reset_opt(#manrec{schduler_ref = test}, #{}).

test_get_discard_opt(_Config) ->
    ?DEFAULT_DISCARD_EVENTS = blockade_service:get_discard_opt(#manrec{}, #{}),
    true = blockade_service:get_discard_opt(#manrec{}, #{discard_events => true}),
    false = blockade_service:get_discard_opt(#manrec{}, #{discard_events => false}),
    true =
        blockade_service:get_discard_opt(#manrec{discard_events = false},
                                         #{discard_events => true}),
    true = blockade_service:get_discard_opt(#manrec{discard_events = true}, #{}).

test_queue_prune(_Config) ->
    Events =
        [{test, "data1", #{priority => 1}},
         {test, "data1_ex", #{priority => 1}},
         {test, "data2", #{priority => 2}},
         {test, "data3", #{priority => 3}},
         {test, "data3_ex", #{priority => 3}}],
    State1 = #manrec{priority = 3, discard_events = false, event_queue = Events},
    State1 = blockade_service:queue_prune(State1),
    #manrec{priority = -15} = blockade_service:queue_prune(State1#manrec{priority = -15}),
    State2 = #manrec{priority = 2, discard_events = true, event_queue = Events},
    #manrec{priority = 2, discard_events = true,
            event_queue =
                [{test, "data2", #{priority := 2}},
                 {test, "data3", #{priority := 3}},
                 {test, "data3_ex", #{priority := 3}}]} =
        blockade_service:queue_prune(State2),
    State3 = #manrec{priority = -150, discard_events = true, event_queue = Events},
    State3 = blockade_service:queue_prune(State3).

test_member_pids(Config) ->
    Self = self(),
    Scope = ?PROCESS_NAME(test_member_pids, "pg"),
    [erpc:call(Node,
               fun() ->
                  gen_server:call(blockade_test_worker, {add_handler, test_member_pids, test_event})
               end)
     || {_, _, Node} <- ?config(nodes, Config)],
    [] = blockade_service:member_pids(Scope, test_event, local),
    blockade:add_handler(test_member_pids, test_event),
    [Self] = blockade_service:member_pids(Scope, test_event, local),
    GlobalMembers = blockade_service:member_pids(Scope, test_event, global),
    ?NR_OF_NODES + 1 = length(GlobalMembers),
    true = lists:all(fun(Pid) -> is_pid(Pid) end, GlobalMembers).
