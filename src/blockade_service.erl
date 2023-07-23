-module(blockade_service).

-include("include/blockade_header.hrl").

-export([rand_node/0, member_pids/3, send_messages/3, dispatch_event/4, queue_event/4,
         dispatch_queued/4, queue_prune/2, get_discard_opt/2, get_reset_opt/2, emit_priority/2,
         sync_priority/2, startup_prio_confr/1, cancel_ref/1]).

%%------------------------------------------------------------------------------
%% Public functions.
%%------------------------------------------------------------------------------

cancel_ref(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_ref(_) ->
    {error, no_ref}.

sync_priority([], Default) ->
    Default;
sync_priority(Priorities, _Default) ->
    [SelectedPrio | _] = most(Priorities),
    SelectedPrio.

emit_priority(Man, Priority) ->
    gen_server:abcast(nodes(), Man, {priority_emit, Priority}),
    ok.

startup_prio_confr(Opts) ->
    maps:get(priority, Opts, false) =/= false orelse length(nodes()) < 0.

dispatch_queued([], _, _, Eq) ->
    lists:reverse(Eq);
dispatch_queued([{Event, Payload, #{priority := Ep} = Opts} | Events], Man, Prio, Eq)
    when Ep >= Prio ->
    dispatch_event(Event, Payload, Man, Opts),
    dispatch_queued(Events, Man, Prio, Eq);
dispatch_queued([Event | Events], Man, Prio, Eq) ->
    dispatch_queued(Events, Man, Prio, [Event | Eq]).

queue_event(EventQueue, {_, _, #{priority := Ep}}, ManPrio, true) when Ep < ManPrio ->
    {event_discarded, EventQueue};
queue_event(EventQueue, {_, _, #{priority := Ep, discard_event := true}}, ManPrio, false)
    when Ep < ManPrio ->
    {event_discarded, EventQueue};
queue_event(EventQueue, {Event, Payload, Opts}, _ManPrio, _DiscardEvents) ->
    Neq = [{Event, Payload, Opts} | EventQueue],
    {event_queued, Neq}.

dispatch_event(Event, Payload, Man, Opts) ->
    Mt = maps:get(members, Opts, global),
    Scope = ?PROCESS_NAME(Man, "pg"),
    Pids = blockade_service:member_pids(Scope, Event, Mt),
    blockade_service:send_messages(Pids, Event, Payload).

send_messages([], _Event, _Payload) ->
    ok;
send_messages([Pid | Pids], Event, Payload) ->
    Pid ! {Event, Payload},
    send_messages(Pids, Event, Payload).

rand_node() ->
    Nodes = erlang:nodes(),
    lists:nth(
        rand:uniform(length(Nodes)), Nodes).

member_pids(Scope, Event, MemberType) when MemberType == local ->
    pg:get_local_members(Scope, Event);
member_pids(Scope, Event, MemberType) when MemberType == global ->
    pg:get_members(Scope, Event);
member_pids(_Scope, _Event, _MemberType) ->
    throw({error, invalid_members_option}).

queue_prune(EventQueue, Priority) ->
    [Event || {_, _, #{priority := EventPrio}} = Event <- EventQueue, EventPrio >= Priority].

get_discard_opt(Opts, DefaultDiscard) ->
    case maps:get(discard_events, Opts, undefined) of
        undefined ->
            DefaultDiscard;
        DiscardEvents ->
            DiscardEvents
    end.

get_reset_opt(Opts, DefaultRef) ->
    case maps:get(reset_after, Opts, undefined) of
        undefined ->
            DefaultRef;
        _ ->
            schedule_reset(Opts)
    end.

%%------------------------------------------------------------------------------
%% Internal functions.
%%------------------------------------------------------------------------------

schedule_reset(Opts) ->
    ResetAfter = maps:get(reset_after, Opts, undefined),
    case ResetAfter of
        undefined ->
            undefined;
        _ ->
            erlang:send_after(ResetAfter, self(), reset_priority)
    end.

most(List) ->
    Lc = lists:foldl(fun(E1, E2) -> maps:put(E1, maps:get(E1, E2, 0) + 1, E2) end, #{}, List),
    Ls = lists:sort(fun({_, Ac}, {_, Bc}) -> Ac > Bc end, maps:to_list(Lc)),
    [Value || {Value, _Count} <- Ls].
