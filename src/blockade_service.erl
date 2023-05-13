-module(blockade_service).

-include("include/blockade_header.hrl").

-export([rand_node/0, member_pids/3, send_messages/3, dispatch_event/4, queue_event/4,
         dispatch_queued/4, queue_prune/1, get_discard_opt/2, get_reset_opt/2, emit_priority/2,
         sync_priority/2]).

%%------------------------------------------------------------------------------
%% Public functions.
%%------------------------------------------------------------------------------

sync_priority([], Default) ->
    Default;
sync_priority(Priorities, _Default) ->
    [SelectedPrio | _] = most(Priorities),
    SelectedPrio.

emit_priority(Man, Priority) ->
    gen_server:abcast(nodes(), Man, {priority_emit, Priority}),
    ok.

dispatch_queued([], _, _, Eq) ->
    lists:reverse(Eq);
dispatch_queued([{Event, Payload, #{priority := Ep} = Opts} | Events], Man, Prio, Eq)
    when Ep >= Prio ->
    dispatch_event(Event, Payload, Man, Opts),
    dispatch_queued(Events, Man, Prio, Eq);
dispatch_queued([Event | Events], Man, Prio, Eq) ->
    dispatch_queued(Events, Man, Prio, [Event | Eq]).

queue_event(_, _, #{priority := Ep}, #manst{discard_events = true, priority = P} = State)
    when Ep < P ->
    {event_discarded, State};
queue_event(Event, Payload, Opts, State) ->
    Neq = [{Event, Payload, Opts} | State#manst.event_queue],
    {event_queued, State#manst{event_queue = Neq}}.

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

queue_prune(#manst{priority = P, discard_events = true, event_queue = Eq} = State) ->
    Neq = [Ed || {_, _, #{priority := Ep}} = Ed <- Eq, Ep >= P],
    State#manst{event_queue = Neq};
queue_prune(State) ->
    State.

get_discard_opt(State, Opts) ->
    case maps:get(discard_events, Opts, undefined) of
        undefined ->
            State#manst.discard_events;
        _ ->
            maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS)
    end.

get_reset_opt(State, Opts) ->
    case maps:get(reset_after, Opts, undefined) of
        undefined ->
            State#manst.schduler_ref;
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
