-module(blockade_event_manager).

-include("blockade_header.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

%%------------------------------------------------------------------------------
%% Record definitions
%%------------------------------------------------------------------------------
-record(state,
        {manager :: blockade:event_manager(),
         event_queue = [] :: list(),
         priority = ?DEFAULT_PRIORITY :: integer(),
         schduler_ref = undefined :: reference() | undefined,
         discard_events = ?DEFAULT_DISCARD_EVENTS}).

%%------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------
start_link(#{name := Name} = Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [Args], []).

%%------------------------------------------------------------------------------
%% Callbacks
%%------------------------------------------------------------------------------
init(Opts) ->
    erlang:process_send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    erlang:process_send_after(?EVENT_QUEUE_SYNC, self(), queue_sync),
    {ok,
     #state{manager = maps:get(name, Opts),
            discard_events =
                maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS),
            priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY)}}.

handle_cast({dispatch, Event, Payload, #{priority := Priority} = Opts}, State)
    when Priority >= State#state.priority ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {noreply, State};
handle_cast({dispatch, Event, Payload, Opts}, State) ->
    {_Resp, NewState} = queue_event(Event, Payload, Opts, State),
    {noreply, NewState};
handle_cast({set_priority, PriorityLvl, Opts},
            #state{event_queue = EventQueue, manager = Manager} = State) ->
    NewEventQueue =
        dispatch_queued(lists:reverse(EventQueue), Manager, PriorityLvl, []),
    {noreply,
     State#state{priority = PriorityLvl,
                 schduler_ref = schedule_reset(Opts),
                 event_queue = NewEventQueue,
                 discard_events =
                     maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {Resp, NewState} = queue_event(Event, Payload, Opts, State),
    {reply, {ok, Resp}, NewState};
handle_call(get_priority, _From, #state{priority = Priority} = State) ->
    {reply, {ok, Priority}, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_info(reset_priority,
            #state{event_queue = EventQueue, manager = Manager} = State) ->
    NewEventQueue =
        dispatch_queued(lists:reverse(EventQueue),
                        Manager,
                        ?DEFAULT_PRIORITY,
                        []),
    {noreply,
     State#state{priority = ?DEFAULT_PRIORITY,
                 schduler_ref = undefined,
                 event_queue = NewEventQueue}};
handle_info(queue_prune, State) ->
    erlang:process_send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    {noreply, queue_prune(State)};
handle_info(queue_sync, State) ->
    erlang:process_send_after(?EVENT_QUEUE_SYNC, self(), queue_sync),
    {noreply, queue_sync(State)};
handle_info(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

dispatch_event(Event, Payload, Manager, Opts) ->
    Members = maps:get(members, Opts, global),

    Pids =
        case Members of
            local ->
                pg:get_local_members(Manager);
            global ->
                pg:get_members(Manager);
            _ ->
                throw({error, invalid_members_option})
        end,
    send_messages(Pids, Event, Payload).

send_messages([], _Event, _Payload) ->
    ok;
send_messages([Pid | Pids], Event, Payload) ->
    Pid ! {Event, Payload},
    send_messages(Pids, Event, Payload).

queue_event(_Event,
            _Payload,
            #{priority := EventPriority},
            #state{discard_events = true, priority = Priority} = State)
    when EventPriority < Priority ->
    {event_discarded, State};
queue_event(Event, Payload, Opts, State) ->
    NewQueue = [{Event, Payload, Opts} | State#state.event_queue],
    {event_queued, State#state{event_queue = NewQueue}}.

dispatch_queued([], _Manager, _Priority, EventsKept) ->
    lists:reverse(EventsKept);
dispatch_queued([{Event, Payload, #{priority := EventPriority} = Opts}
                 | Events],
                Manager,
                Priority,
                EventsKept)
    when EventPriority >= Priority ->
    dispatch_event(Event, Payload, Manager, Opts),
    dispatch_queued(Events, Manager, Priority, EventsKept);
dispatch_queued([Event | Events], Manager, Priority, EventsKept) ->
    dispatch_queued(Events, Manager, Priority, [Event | EventsKept]).

schedule_reset(Opts) ->
    ResetAfter = maps:get(reset_after, Opts, undefined),
    case ResetAfter of
        undefined ->
            undefined;
        _ ->
            erlang:process_send_after(ResetAfter, self(), reset_priority)
    end.

queue_prune(#state{priority = Priority,
                   discard_events = true,
                   event_queue = EventQueue} =
                State) ->
    NewEventQueue =
        [EventData
         || {_, _, #{priority := EventPriority}} = EventData <- EventQueue,
            EventPriority >= Priority],
    State#state{event_queue = NewEventQueue};
queue_prune(State) ->
    State.

queue_sync(#state{priority = LocalPriority} = State) ->
    AgreedPriority =
        case remote_priority() of
            LocalPriority ->
                LocalPriority;
            RemotePriority ->
                % Check once again.
                RemotePrioritySec = remote_priority(),
                [SelectedPrio | _] =
                    most([LocalPriority, RemotePriority, RemotePrioritySec]),
                SelectedPrio
        end,
    State#state{priority = AgreedPriority}.

remote_priority() ->
    Nodes = erlang:nodes(),
    RandomNode =
        lists:nth(
            rand:uniform(length(Nodes)), Nodes),
    gen_server:call({RandomNode, ?MODULE}, get_priority, ?GEN_CALL_TIMEOUT).

most(List) ->
    ListCounted =
        lists:foldl(fun(El, Acc) -> maps:put(El, maps:get(El, Acc, 0) + 1, Acc)
                    end,
                    #{},
                    List),
    ListSorted =
        lists:sort(fun({_, Acount}, {_, Bcount}) -> Acount > Bcount end,
                   maps:to_list(ListCounted)),
    [Value || {Value, _Count} <- ListSorted].
