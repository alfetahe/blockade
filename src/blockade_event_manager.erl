%%%-----------------------------------------------------------------------------
%% @doc The event queue manager.
%% @private
%% @end
%%%-----------------------------------------------------------------------------
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
         event_queue = [] :: [blockade:queued_event()],
         priority = ?DEFAULT_PRIORITY :: integer(),
         discard_events = ?DEFAULT_DISCARD_EVENTS,
         schduler_ref = undefined :: reference() | undefined,
         emitted_priorities = [] :: [blockade:priority()],
         priority_confirmed = false :: true | false,
         priority_sync = true :: true | false}).

%%------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------
start_link(#{name := Name} = Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%------------------------------------------------------------------------------
%% Callbacks
%%------------------------------------------------------------------------------
init(Opts) ->
    PrioSync = maps:get(priority_sync, Opts, true),

    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),

    if PrioSync =:= true ->
           erlang:send_after(?PRIORITY_EMIT_SCHEDULE, self(), emit_priority),
           erlang:send_after(?PRIORITY_SYNC_SCHEDULE, self(), sync_priority);
       true ->
           ok
    end,

    {ok,
     #state{manager = maps:get(name, Opts),
            discard_events = maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS),
            priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY),
            emitted_priorities = [],
            priority_confirmed = blockade_service:startup_prio_confr(Opts),
            priority_sync = PrioSync}}.

handle_cast({discard_events, true}, #state{priority = P, event_queue = Eq} = State) ->
    NewEq = blockade_service:queue_prune(Eq, P),
    {noreply, State#state{discard_events = true, event_queue = NewEq}};
handle_cast({discard_events, false}, State) ->
    {noreply, State#state{discard_events = false}};
handle_cast({dispatch, Event, Payload, #{priority := Ep} = Opts},
            #state{manager = M, priority = Lp} = State)
    when Ep >= Lp ->
    blockade_service:dispatch_event(Event, Payload, M, Opts),
    NewPriority = blockade_service:atomic_priority_update(M, Lp, Opts),
    {noreply, State#state{priority = NewPriority}};
handle_cast({dispatch, Event, Payload, Opts},
            #state{event_queue = Eq, priority = P, discard_events = De} = State) ->
    {_Resp, Neq} = blockade_service:queue_event(Eq, {Event, Payload, Opts}, P, De),
    {noreply, State#state{event_queue = Neq}};
handle_cast(prune_event_queue, State) ->
    {noreply, State#state{event_queue = []}};
handle_cast({priority_emit, EmittedPrio}, #state{emitted_priorities = Ep} = State) ->
    {noreply, State#state{emitted_priorities = [EmittedPrio | Ep]}};
handle_cast({set_priority, Priority, Opts},
            #state{event_queue = Eq, manager = Man, schduler_ref = Sr} = State) ->
    blockade_service:cancel_ref(Sr),
    Neq = blockade_service:dispatch_queued(
              lists:reverse(Eq), Man, Priority, []),
    Nde = blockade_service:get_discard_opt(Opts, State#state.discard_events),
    Nsr = blockade_service:get_reset_opt(Opts, Sr),
    {noreply,
     State#state{priority = Priority, schduler_ref = Nsr, event_queue = Neq,
                 discard_events = Nde}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({dispatch, Event, Payload, #{priority := P} = Opts},
            _From,
            #state{manager = M} = State)
    when P >= State#state.priority ->
    blockade_service:dispatch_event(Event, Payload, State#state.manager, Opts),
    NewPriority = blockade_service:atomic_priority_update(M, State#state.priority, Opts),
    {reply, {ok, event_dispatched}, State#state{priority = NewPriority}};
handle_call({dispatch, Event, Payload, Opts},
            _From,
            #state{priority = P, event_queue = Eq, discard_events = De} = State) ->
    {Resp, NewEq} = blockade_service:queue_event(Eq, {Event, Payload, Opts}, P, De),
    {reply, {ok, Resp}, State#state{event_queue = NewEq}};
handle_call(get_priority, _From, #state{priority = Priority} = State) ->
    {reply, {ok, Priority}, State};
handle_call(get_event_queue, _From, #state{event_queue = Eq} = State) ->
    {reply, {ok, lists:reverse(Eq)}, State};
handle_call(get_state, _From, State) ->
    StateMap =
        #{manager => State#state.manager,
          priority => State#state.priority,
          event_queue => lists:reverse(State#state.event_queue),
          discard_events => State#state.discard_events,
          schduler_ref => State#state.schduler_ref,
          emitted_priorities => State#state.emitted_priorities,
          priority_confirmed => State#state.priority_confirmed,
          priority_sync => State#state.priority_sync},
    {reply, StateMap, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_info(sync_priority, #state{priority_sync = false} = State) ->
    {noreply, State};
handle_info(sync_priority,
            #state{emitted_priorities = Ep, priority = Priority} = State) ->
    SyncPrio = blockade_service:sync_priority(Ep, Priority),
    erlang:send_after(?PRIORITY_SYNC_SCHEDULE, self(), sync_priority),
    {noreply,
     State#state{emitted_priorities = [], priority = SyncPrio, priority_confirmed = true}};
handle_info(emit_priority, #state{priority_sync = false} = State) ->
    {noreply, State};
handle_info(emit_priority, #state{priority_confirmed = false} = State) ->
    erlang:send_after(?PRIORITY_EMIT_SCHEDULE, self(), emit_priority),
    {noreply, State};
handle_info(emit_priority, #state{priority = Priority, manager = Man} = State) ->
    blockade_service:emit_priority(Man, Priority),
    erlang:send_after(?PRIORITY_EMIT_SCHEDULE, self(), emit_priority),
    {noreply, State};
handle_info(queue_prune,
            #state{priority = P, event_queue = Eq, discard_events = true} = State) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    NewEq = blockade_service:queue_prune(Eq, P),
    {noreply, State#state{event_queue = NewEq}};
handle_info(queue_prune, #state{discard_events = false} = State) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    {noreply, State};
handle_info(reset_priority, #state{event_queue = Eq, manager = Man} = State) ->
    Neq = blockade_service:dispatch_queued(
              lists:reverse(Eq), Man, ?DEFAULT_PRIORITY, []),
    {noreply,
     State#state{priority = ?DEFAULT_PRIORITY, schduler_ref = undefined, event_queue = Neq}};
handle_info(_Msg, State) ->
    {noreply, State}.
