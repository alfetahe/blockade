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
    erlang:process_send_after(?EVENT_QUEUE_PRUNE, self(), prune_queue),
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
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({set_priority, PriorityLvl, Opts},
            _From,
            #state{event_queue = EventQueue, manager = Manager} = State) ->
    NewEventQueue =
        dispatch_queued(lists:reverse(EventQueue), Manager, PriorityLvl, []),
    {reply,
     {ok, priority_set},
     State#state{priority = PriorityLvl,
                 schduler_ref = schedule_reset(Opts),
                 event_queue = NewEventQueue,
                 discard_events =
                     maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS)}};
handle_call({dispatch, Event, Payload, #{priority := Priority} = Opts},
            _From,
            State)
    when Priority >= State#state.priority ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {reply, {ok, event_dispatched}, State};
handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {Resp, NewState} = queue_event(Event, Payload, Opts, State),
    {reply, {ok, Resp}, NewState};
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
handle_info(prune_queue, State) ->
    erlang:process_send_after(?EVENT_QUEUE_PRUNE, self(), prune_queue),
    {noreply, prune_queue(State)};
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

prune_queue(#state{priority = Priority,
                   discard_events = true,
                   event_queue = EventQueue} =
                State) ->
    NewEventQueue =
        [EventData
         || {_, _, #{priority := EventPriority}} = EventData <- EventQueue,
            EventPriority >= Priority],
    State#state{event_queue = NewEventQueue};
prune_queue(State) ->
    State.
