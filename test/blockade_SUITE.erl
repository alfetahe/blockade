-module(blockade_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_add_handler/1, test_remove_handler/1, test_get_events/1,
         test_get_handlers/1, test_get_priority/1, test_set_priority/1, test_get_event_queue/1,
         test_prune_event_queue/1]).

all() ->
    [test_add_handler,
     test_remove_handler,
     test_get_events,
     test_get_handlers,
     test_get_priority,
     test_set_priority,
     test_get_event_queue,
     test_prune_event_queue].

init_per_testcase(TestCase, Config) ->
    blockade_sup:start_link(#{name => TestCase}),
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
    {ok, [{test_event, [], #{priority := 9}}, {test_event, [], #{priority := 0}}]} =
        blockade:get_event_queue(test_get_event_queue).

test_prune_event_queue(_Config) ->
    {ok, []} = blockade:get_event_queue(test_prune_event_queue),
    blockade:dispatch(test_prune_event_queue, test_event, [], #{priority => -1}),
    {ok, [{test_event, [], #{priority := -1}}]} =
        blockade:get_event_queue(test_prune_event_queue),
    ok = blockade:prune_event_queue(test_prune_event_queue),
    {ok, []} = blockade:get_event_queue(test_prune_event_queue).
