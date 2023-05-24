-module(blockade_event_manager).

-include("include/blockade_header.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

%%------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------
start_link(#{name := Name} = Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%------------------------------------------------------------------------------
%% Callbacks
%%------------------------------------------------------------------------------
init(Opts) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    erlang:send_after(?PRIORITY_EMIT_SCHEDULE, self(), emit_priority),
    erlang:send_after(?PRIORITY_SYNC_SCHEDULE, self(), sync_priority),
    {ok,
     #manst{manager = maps:get(name, Opts),
            discard_events = maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS),
            priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY),
            emitted_priorites = [],
            priority_confirmed = blockade_service:startup_prio_confr(Opts)}}.

handle_cast({discard_events, true}, #manst{priority = P, event_queue = Eq} = State) ->
    NewEq = blockade_service:queue_prune(Eq, P),
    {noreply, State#manst{discard_events = true, event_queue = NewEq}};
handle_cast({discard_events, false}, State) ->
    {noreply, State#manst{discard_events = false}};
handle_cast({dispatch, Event, Payload, #{priority := P} = Opts}, State)
    when P >= State#manst.priority ->
    blockade_service:dispatch_event(Event, Payload, State#manst.manager, Opts),
    {noreply, State};
handle_cast({dispatch, Event, Payload, Opts}, State) ->
    {_Resp, NewState} = blockade_service:queue_event(Event, Payload, Opts, State),
    {noreply, NewState};
handle_cast(prune_event_queue, State) ->
    {noreply, State#manst{event_queue = []}};
handle_cast({priority_emit, EmittedPrio}, #manst{emitted_priorites = Ep} = State) ->
    {noreply, State#manst{emitted_priorites = [EmittedPrio | Ep]}};
handle_cast({set_priority, Priority, Opts},
            #manst{event_queue = Eq, manager = Man} = State) ->
    {noreply,
     State#manst{priority = Priority,
                 schduler_ref = blockade_service:get_reset_opt(State, Opts),
                 event_queue =
                     blockade_service:dispatch_queued(
                         lists:reverse(Eq), Man, Priority, []),
                 discard_events = blockade_service:get_discard_opt(State, Opts)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({dispatch, Event, Payload, #{priority := P} = Opts}, _From, State)
    when P >= State#manst.priority ->
    blockade_service:dispatch_event(Event, Payload, State#manst.manager, Opts),
    {reply, {ok, event_dispatched}, State};
handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {Resp, NewState} = blockade_service:queue_event(Event, Payload, Opts, State),
    {reply, {ok, Resp}, NewState};
handle_call(get_priority, _From, #manst{priority = Priority} = State) ->
    {reply, {ok, Priority}, State};
handle_call(get_event_queue, _From, #manst{event_queue = Eq} = State) ->
    {reply, {ok, lists:reverse(Eq)}, State};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_info(sync_priority, #manst{emitted_priorites = Ep, priority = Priority} = State) ->
    SyncPrio = blockade_service:sync_priority(Ep, Priority),
    erlang:send_after(?PRIORITY_SYNC_SCHEDULE, self(), sync_priority),
    {noreply,
     State#manst{emitted_priorites = [], priority = SyncPrio, priority_confirmed = true}};
handle_info(emit_priority, #manst{priority_confirmed = Confirm} = State)
    when Confirm =:= false ->
    erlang:send_after(?PRIORITY_EMIT_SCHEDULE, self(), emit_priority),
    {noreply, State};
handle_info(emit_priority, #manst{priority = Priority, manager = Man} = State) ->
    blockade_service:emit_priority(Man, Priority),
    erlang:send_after(?PRIORITY_EMIT_SCHEDULE, self(), emit_priority),
    {noreply, State};
handle_info(queue_prune,
            #manst{priority = P, event_queue = Eq, discard_events = true} = State) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    NewEq = blockade_service:queue_prune(Eq, P),
    {noreply, State#manst{event_queue = NewEq}};
handle_info(queue_prune, #manst{discard_events = false} = State) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    {noreply, State};
handle_info(reset_priority, #manst{event_queue = Eq, manager = Man} = State) ->
    Neq = blockade_service:dispatch_queued(
              lists:reverse(Eq), Man, ?DEFAULT_PRIORITY, []),
    {noreply,
     State#manst{priority = ?DEFAULT_PRIORITY, schduler_ref = undefined, event_queue = Neq}};
handle_info(_Msg, State) ->
    {noreply, State}.
