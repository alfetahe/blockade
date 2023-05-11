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
    {ok,
     #manrec{manager = maps:get(name, Opts),
             discard_events = maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS),
             priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY)}}.

handle_cast({dispatch, Event, Payload, #{priority := P} = Opts}, State)
    when P >= State#manrec.priority ->
    blockade_service:dispatch_event(Event, Payload, State#manrec.manager, Opts),
    {noreply, State};
handle_cast({dispatch, Event, Payload, Opts}, State) ->
    {_Resp, NewState} = blockade_service:queue_event(Event, Payload, Opts, State),
    {noreply, NewState};
handle_cast(prune_event_queue, State) ->
    {noreply, State#manrec{event_queue = []}};
handle_cast({set_priority, Priority, Opts},
            #manrec{event_queue = Eq, manager = Man} = State) ->
    {noreply,
     State#manrec{priority = Priority,
                  schduler_ref = blockade_service:get_reset_opt(State, Opts),
                  event_queue =
                      blockade_service:dispatch_queued(
                          lists:reverse(Eq), Man, Priority, []),
                  discard_events = blockade_service:get_discard_opt(State, Opts)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({dispatch, Event, Payload, #{priority := P} = Opts}, _From, State)
    when P >= State#manrec.priority ->
    blockade_service:dispatch_event(Event, Payload, State#manrec.manager, Opts),
    {reply, {ok, event_dispatched}, State};
handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {Resp, NewState} = blockade_service:queue_event(Event, Payload, Opts, State),
    {reply, {ok, Resp}, NewState};
handle_call(get_priority, _From, #manrec{priority = Priority} = State) ->
    {reply, {ok, Priority}, State};
handle_call(get_event_queue, _From, #manrec{event_queue = Eq} = State) ->
    {reply, {ok, Eq}, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_info(queue_prune, State) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    {noreply, blockade_service:queue_prune(State)};
handle_info(reset_priority, #manrec{event_queue = Eq, manager = Man} = State) ->
    Neq = blockade_service:dispatch_queued(
              lists:reverse(Eq), Man, ?DEFAULT_PRIORITY, []),
    {noreply,
     State#manrec{priority = ?DEFAULT_PRIORITY, schduler_ref = undefined, event_queue = Neq}};
handle_info(_Msg, State) ->
    {noreply, State}.
