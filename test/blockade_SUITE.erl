-module(blockade_SUITE).

-behaviour(ct_suite).

-include("../include/blockade_header.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_add_handler/1, test_remove_handler/1, test_get_events/1,
         test_get_handlers/1, test_get_priority/1, test_set_priority/1, test_get_event_queue/1,
         test_prune_event_queue/1, test_dispatch/1, test_dispatch_priority/1, test_dispatch_sync/1,
         test_discard_events/1, test_discard_event/1, test_local_manager_state/1,
         test_atomic_priority_update/1]).

all() ->
    [test_add_handler,
     test_remove_handler,
     test_get_events,
     test_get_handlers,
     test_get_priority,
     test_set_priority,
     test_get_event_queue,
     test_prune_event_queue,
     test_dispatch,
     test_dispatch_priority,
     test_dispatch_sync,
     test_discard_events,
     test_discard_event,
     test_local_manager_state,
     test_atomic_priority_update].

init_per_testcase(TestCase, Config) ->
    blockade_sup:start_link(TestCase, #{priority => ?DEFAULT_PRIORITY}),
    Config.

end_per_testcase(TestCase, _Config) ->
    blockade_sup:stop(TestCase).

test_add_handler(_Config) ->
    blockade:add_handler(test_add_handler, test_event),
    Pid = self(),
    {ok, [Pid]} = blockade:get_handlers(test_add_handler, test_event),
    blockade:add_handler(test_add_handler, test_event),
    {ok, [Pid, Pid]} = blockade:get_handlers(test_add_handler, test_event).

test_remove_handler(_Config) ->
    blockade:add_handler(test_remove_handler, test_event),
    Pid = self(),
    {ok, [Pid]} = blockade:get_handlers(test_remove_handler, test_event),
    blockade:remove_handler(test_remove_handler, test_event),
    {ok, []} = blockade:get_handlers(test_remove_handler, test_event).

test_get_events(_Config) ->
    {ok, []} = blockade:get_events(test_get_events),
    blockade:add_handler(test_get_events, test_event),
    {ok, [test_event]} = blockade:get_events(test_get_events).

test_get_handlers(_Config) ->
    blockade:add_handler(test_get_handlers, test_event),
    Pid = self(),
    {ok, [Pid]} = blockade:get_handlers(test_get_handlers, test_event).

test_get_priority(_Config) ->
    {ok, 0} = blockade:get_priority(test_get_priority),
    blockade:set_priority(test_get_priority, 1, #{}),
    {ok, 1} = blockade:get_priority(test_get_priority).

test_set_priority(_Config) ->
    blockade:set_priority(test_set_priority, -1, #{}),
    {ok, -1} = blockade:get_priority(test_set_priority),
    blockade:set_priority(test_set_priority, 3500, #{}),
    {ok, 3500} = blockade:get_priority(test_set_priority),
    {error, priority_not_integer} = blockade:set_priority(test_set_priority, 1.5, #{}).

test_get_event_queue(_Config) ->
    {ok, []} = blockade:get_event_queue(test_get_event_queue),
    blockade:set_priority(test_get_event_queue, 10, #{}),
    blockade:dispatch(test_get_event_queue, test_event, [], #{}),
    blockade:dispatch(test_get_event_queue, test_event, [], #{priority => 9}),
    {ok, [{test_event, [], #{priority := 0}}, {test_event, [], #{priority := 9}}]} =
        blockade:get_event_queue(test_get_event_queue).

test_prune_event_queue(_Config) ->
    blockade:dispatch(test_prune_event_queue, test_event, [], #{priority => -1}),
    {ok, [{test_event, [], #{priority := -1}}]} =
        blockade:get_event_queue(test_prune_event_queue),
    ok = blockade:prune_event_queue(test_prune_event_queue),
    {ok, []} = blockade:get_event_queue(test_prune_event_queue).

test_dispatch(_Config) ->
    blockade:add_handler(test_dispatch, test_event),
    blockade:dispatch(test_dispatch, test_event, test_data, #{}),
    ok =
        receive
            {test_event, test_data} ->
                ok
        after 1000 ->
            error
        end.

test_dispatch_priority(_Config) ->
    blockade:add_handler(test_dispatch_priority, test_event),
    blockade:set_priority(test_dispatch_priority, 10, #{}),
    blockade:dispatch(test_dispatch_priority, test_event, queued_msg, #{priority => 5}),
    blockade:dispatch(test_dispatch_priority, test_event, first_msg, #{priority => 10}),
    blockade:dispatch(test_dispatch_priority, test_event, second_msg, #{priority => 11}),
    % Need to call one sync event to ensure that the events are dispatched.
    blockade:get_event_queue(test_dispatch_priority),
    [{test_event, first_msg}, {test_event, second_msg}] = get_messages().

test_dispatch_sync(_Config) ->
    blockade:add_handler(test_dispatch_sync, test_event),
    {ok, event_dispatched} =
        blockade:dispatch_sync(test_dispatch_sync, test_event, test_data, #{priority => 5}),
    ok =
        receive
            {test_event, test_data} ->
                ok
        after 1000 ->
            error
        end.

test_discard_events(_Config) ->
    blockade:dispatch(test_discard_events, test_event, test_data, #{priority => -5}),
    blockade:discard_events(test_discard_events, true),
    ManState =
        blockade:local_manager_state(test_discard_events), % Do sync call and get state.
    true = maps:get(discard_events, ManState, false),
    {ok, []} = blockade:get_event_queue(test_discard_events).

test_discard_event(_Config) ->
    blockade:dispatch(test_discard_event,
                      test_event,
                      test_data,
                      #{priority => -10, discard_event => true}),
    blockade:discard_events(test_discard_event, false),
    ManState =
        blockade:local_manager_state(test_discard_event), % Do sync call and get state.
    false = maps:get(discard_events, ManState, false),
    {ok, []} = blockade:get_event_queue(test_discard_event).

test_local_manager_state(_Config) ->
    LocalManState = blockade:local_manager_state(test_local_manager_state),
    LocalManState =
        #{manager => test_local_manager_state,
          discard_events => ?DEFAULT_DISCARD_EVENTS,
          priority => ?DEFAULT_PRIORITY,
          event_queue => [],
          schduler_ref => undefined,
          emitted_priorities => [],
          priority_confirmed => true,
          priority_sync => true}.

get_messages() ->
    {messages, Messages} = process_info(self(), messages),
    Messages.

test_atomic_priority_update(_Config) ->
    blockade:dispatch_sync(test_atomic_priority_update,
                           test_event,
                           test_data,
                           #{atomic_priority_set => -15}),
    {ok, -15} = blockade:get_priority(test_atomic_priority_update).
