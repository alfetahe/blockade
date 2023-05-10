-module(blockade_event_manager).

-include("include/blockade_header.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

%%--------------------------------------------------------------------------------------------------
%% Record definitions
%%--------------------------------------------------------------------------------------------------
-record(state,
        {manager :: blockade:event_manager(),
         event_queue = [] :: list(),
         priority = ?DEFAULT_PRIORITY :: integer(),
         discard_events = ?DEFAULT_DISCARD_EVENTS,
         schduler_ref = undefined :: reference() | undefined}).

%%--------------------------------------------------------------------------------------------------
%% Internal API
%%--------------------------------------------------------------------------------------------------
start_link(#{name := Name} = Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%--------------------------------------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------------------------------------
init(Opts) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    {ok,
     #state{manager = maps:get(name, Opts),
            discard_events = maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS),
            priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY)}}.

handle_cast({dispatch, Event, Payload, #{priority := P} = Opts}, State)
    when P >= State#state.priority ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {noreply, State};
handle_cast({dispatch, Event, Payload, Opts}, State) ->
    {_Resp, NewState} = queue_event(Event, Payload, Opts, State),
    {noreply, NewState};
handle_cast(prune_event_queue, State) ->
    {noreply, State#state{event_queue = []}};
handle_cast({set_priority, Priority, Opts},
            #state{event_queue = Eq, manager = Man} = State) ->
    {noreply,
     State#state{priority = Priority,
                 schduler_ref = get_reset_opt(State, Opts),
                 event_queue = dispatch_queued(lists:reverse(Eq), Man, Priority, []),
                 discard_events = get_discard_opt(State, Opts)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({dispatch, Event, Payload, #{priority := P} = Opts}, _From, State)
    when P >= State#state.priority ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {reply, {ok, event_dispatched}, State};
handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {Resp, NewState} = queue_event(Event, Payload, Opts, State),
    {reply, {ok, Resp}, NewState};
handle_call(get_priority, _From, #state{priority = Priority} = State) ->
    {reply, {ok, Priority}, State};
handle_call(get_event_queue, _From, #state{event_queue = Eq} = State) ->
    {reply, {ok, Eq}, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_info(queue_prune, State) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    {noreply, queue_prune(State)};
handle_info(reset_priority, #state{event_queue = Eq, manager = Man} = State) ->
    Neq = dispatch_queued(lists:reverse(Eq), Man, ?DEFAULT_PRIORITY, []),
    {noreply,
     State#state{priority = ?DEFAULT_PRIORITY,
                 schduler_ref = undefined,
                 event_queue = Neq}};
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------------------------------------
%% Private functions
%%--------------------------------------------------------------------------------------------------

dispatch_event(Event, Payload, Man, Opts) ->
    Mt = maps:get(members, Opts, global),
    Scope = ?PROCESS_NAME(Man, "pg"),
    Pids = member_pids(Scope, Event, Mt),
    send_messages(Pids, Event, Payload).

member_pids(Scope, Event, MemberType) when MemberType == local ->
    pg:get_local_members(Scope, Event);
member_pids(Scope, Event, MemberType) when MemberType == global ->
    pg:get_members(Scope, Event);
member_pids(_Scope, _Event, _MemberType) ->
    throw({error, invalid_members_option}).

send_messages([], _Event, _Payload) ->
    ok;
send_messages([Pid | Pids], Event, Payload) ->
    Pid ! {Event, Payload},
    send_messages(Pids, Event, Payload).

queue_event(_, _, #{priority := Ep}, #state{discard_events = true, priority = P} = State)
    when Ep < P ->
    {event_discarded, State};
queue_event(Event, Payload, Opts, State) ->
    Neq = [{Event, Payload, Opts} | State#state.event_queue],
    {event_queued, State#state{event_queue = Neq}}.

dispatch_queued([], _, _, Eq) ->
    lists:reverse(Eq);
dispatch_queued([{Event, Payload, #{priority := Ep} = Opts} | Events], Man, Prio, Eq)
    when Ep >= Prio ->
    dispatch_event(Event, Payload, Man, Opts),
    dispatch_queued(Events, Man, Prio, Eq);
dispatch_queued([Event | Events], Man, Prio, Eq) ->
    dispatch_queued(Events, Man, Prio, [Event | Eq]).

schedule_reset(Opts) ->
    ResetAfter = maps:get(reset_after, Opts, undefined),
    case ResetAfter of
        undefined ->
            undefined;
        _ ->
            erlang:send_after(ResetAfter, self(), reset_priority)
    end.

queue_prune(#state{priority = P,
                   discard_events = true,
                   event_queue = Eq} =
                State) ->
    Neq = [Ed || {_, _, #{priority := Ep}} = Ed <- Eq, Ep >= P],
    State#state{event_queue = Neq};
queue_prune(State) ->
    State.

get_discard_opt(State, Opts) ->
    case maps:get(discard_events, Opts, undefined) of
        undefined ->
            State#state.discard_events;
        _ ->
            maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS)
    end.

get_reset_opt(State, Opts) ->
    case maps:get(reset_after, Opts, undefined) of
        undefined ->
            State#state.schduler_ref;
        _ ->
            schedule_reset(Opts)
    end.
