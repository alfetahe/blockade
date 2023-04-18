-module(blockade_event_manager).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

-define(DEFAULT_PRIORITY, 0).

%%------------------------------------------------------------------------------
%% Record definitions
%%------------------------------------------------------------------------------
-record(state,
        {manager :: blockade:event_manager(),
         event_queue = [] :: list(),
         priority = ?DEFAULT_PRIORITY :: integer(),
         schduler_ref = undefined :: reference() | undefined,
         discard_events = false}).

%%------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------
start_link(#{name := Name} = _Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

%%------------------------------------------------------------------------------
%% Callbacks
%%------------------------------------------------------------------------------
init(Name) ->
    {ok, low, #state{manager = Name}}.

handle_cast({dispatch, Event, Payload, #{priority := Priority} = Opts}, State)
    when Priority >= State#state.priority ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {noreply, State};
handle_cast({dispatch, Event, Payload, Opts}, State) ->
    {noreply, queue_event(Event, Payload, Opts, State)};
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
                 event_queue = NewEventQueue}};
handle_call({dispatch, Event, Payload, #{priority := Priority} = Opts},
            _From,
            State)
    when Priority >= State#state.priority ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {reply, {ok, event_dispatched}, State};
handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {reply, {ok, event_queued}, queue_event(Event, Payload, Opts, State)};
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

queue_event(Event, Payload, Opts, State) ->
    NewQueue = [{Event, Payload, Opts} | State#state.event_queue],
    State#state{event_queue = NewQueue}.

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
