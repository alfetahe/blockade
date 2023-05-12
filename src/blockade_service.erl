-module(blockade_service).

-include("include/blockade_header.hrl").

-export([rand_node/0, get_sync_priority/2, member_pids/3, send_messages/3,
         dispatch_event/4, queue_event/4, dispatch_queued/4, queue_prune/1, get_discard_opt/2,
         get_reset_opt/2]).

%%------------------------------------------------------------------------------
%% Public functions.
%%------------------------------------------------------------------------------
get_sync_priority(Lp, Man) ->
    case remote_priority(Man) of
        Lp ->
            Lp;
        undefined ->
            Lp;
        Rp ->
            % If there are only 2 nodes in the cluster then agree with the
            % remote priority. Otherwise, check the remote priority again.
            case length(erlang:nodes()) of
                1 ->
                    Rp;
                _ ->
                    Rp2 = remote_priority(Man),
                    [Sp | _] = most([Lp, Rp, Rp2]),
                    Sp
            end
    end.

queue_event(_, _, #{priority := Ep}, #manrec{discard_events = true, priority = P} = State)
    when Ep < P ->
    {event_discarded, State};
queue_event(Event, Payload, Opts, State) ->
    Neq = [{Event, Payload, Opts} | State#manrec.event_queue],
    {event_queued, State#manrec{event_queue = Neq}}.

dispatch_queued([], _, _, Eq) ->
    lists:reverse(Eq);
dispatch_queued([{Event, Payload, #{priority := Ep} = Opts} | Events], Man, Prio, Eq)
    when Ep >= Prio ->
    dispatch_event(Event, Payload, Man, Opts),
    dispatch_queued(Events, Man, Prio, Eq);
dispatch_queued([Event | Events], Man, Prio, Eq) ->
    dispatch_queued(Events, Man, Prio, [Event | Eq]).

send_messages([], _Event, _Payload) ->
    ok;
send_messages([Pid | Pids], Event, Payload) ->
    Pid ! {Event, Payload},
    send_messages(Pids, Event, Payload).

dispatch_event(Event, Payload, Man, Opts) ->
    Mt = maps:get(members, Opts, global),
    Scope = ?PROCESS_NAME(Man, "pg"),
    Pids = blockade_service:member_pids(Scope, Event, Mt),
    blockade_service:send_messages(Pids, Event, Payload).

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

queue_prune(#manrec{priority = P, discard_events = true, event_queue = Eq} = State) ->
    Neq = [Ed || {_, _, #{priority := Ep}} = Ed <- Eq, Ep >= P],
    State#manrec{event_queue = Neq};
queue_prune(State) ->
    State.

get_discard_opt(State, Opts) ->
    case maps:get(discard_events, Opts, undefined) of
        undefined ->
            State#manrec.discard_events;
        _ ->
            maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS)
    end.

get_reset_opt(State, Opts) ->
    case maps:get(reset_after, Opts, undefined) of
        undefined ->
            State#manrec.schduler_ref;
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

remote_priority(Manager) ->
    Nodes = erlang:nodes(),
    if length(Nodes) == 0 ->
           ?DEFAULT_PRIORITY;
       true ->
           RandNode = rand_node(),
           case erpc:call(RandNode, fun() -> whereis(Manager) end) of
               undefined ->
                   ?DEFAULT_PRIORITY;
               Res ->
                   ct:pal("REMOTE NODE ~p~n", [RandNode]),
                   ct:pal("LOCAL NODE ~p~n", [node()]),
                   ct:pal("GEN PIDD ~p~n", [Res]),
                   query_remote_priority(Manager, RandNode)
           end
    end.

query_remote_priority(Manager, RandNode) ->
    case gen_server:call({Manager, RandNode}, get_priority, ?GEN_CALL_TIMEOUT) of
        undefined ->
            ?DEFAULT_PRIORITY;
        _ ->
            gen_server:call({Manager, RandNode}, get_priority, ?GEN_CALL_TIMEOUT)
    end.

most(List) ->
    Lc = lists:foldl(fun(E1, E2) -> maps:put(E1, maps:get(E1, E2, 0) + 1, E2) end, #{}, List),
    Ls = lists:sort(fun({_, Ac}, {_, Bc}) -> Ac > Bc end, maps:to_list(Lc)),
    [Value || {Value, _Count} <- Ls].
