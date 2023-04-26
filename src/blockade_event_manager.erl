-module(blockade_event_manager).

-include("blockade_header.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, handle_continue/2]).

%%--------------------------------------------------------------------------------------------------
%% Record definitions
%%--------------------------------------------------------------------------------------------------
-record(state,
        {manager :: blockade:event_manager(),
         event_queue = [] :: list(),
         priority = ?DEFAULT_PRIORITY :: integer(),
         schduler_ref = undefined :: reference() | undefined,
         discard_events = ?DEFAULT_DISCARD_EVENTS}).

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
    erlang:send_after(?PRIORITY_SYNC, self(), queue_sync),
    {ok,
     #state{manager = maps:get(name, Opts),
            discard_events = maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS),
            priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY)},
     {continue, priority_init}}.

handle_continue(priority_init, State) ->
    {noreply, priority_sync(State)}.

handle_cast({dispatch, Event, Payload, #{priority := P} = Opts}, State)
    when P >= State#state.priority ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {noreply, State};
handle_cast({dispatch, Event, Payload, Opts}, State) ->
    {_Resp, NewState} = queue_event(Event, Payload, Opts, State),
    {noreply, NewState};
handle_cast({set_priority, Plvl, Opts},
            #state{event_queue = Eq, manager = Man} = State) ->
    Neq = dispatch_queued(lists:reverse(Eq), Man, Plvl, []),
    {noreply,
     State#state{priority = Plvl,
                 schduler_ref = schedule_reset(Opts),
                 event_queue = Neq,
                 discard_events = maps:get(discard_events, Opts, ?DEFAULT_DISCARD_EVENTS)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {Resp, NewState} = queue_event(Event, Payload, Opts, State),
    {reply, {ok, Resp}, NewState};
handle_call(get_priority, _From, #state{priority = Priority} = State) ->
    {reply, {ok, Priority}, State};
handle_call(get_event_queue, _From, #state{event_queue = Eq} = State) ->
    {reply, {ok, Eq}, State};
handle_call(prune_event_queue, _From, State) ->
    {reply, ok, State#state{event_queue = []}};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_info(queue_prune, State) ->
    erlang:send_after(?EVENT_QUEUE_PRUNE, self(), queue_prune),
    {noreply, queue_prune(State)};
handle_info(priority_sync, State) ->
    erlang:send_after(?PRIORITY_SYNC, self(), priority_sync),
    {noreply, priority_sync(State)};
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
    M = maps:get(members, Opts, global),
    Scope = ?PROCESS_NAME(Man, "pg"),
    Pids =
        case M of
            local ->
                pg:get_local_members(Scope, Event);
            global ->
                pg:get_members(Scope, Event);
            _ ->
                throw({error, invalid_members_option})
        end,
    send_messages(Pids, Event, Payload).

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
    Rs = maps:get(reset_after, Opts, undefined),
    case Rs of
        undefined ->
            undefined;
        _ ->
            erlang:send_after(Rs, self(), reset_priority)
    end.

queue_prune(#state{priority = P,
                   discard_events = true,
                   event_queue = Eq} =
                State) ->
    Neq = [Ed || {_, _, #{priority := Ep}} = Ed <- Eq, Ep >= P],
    State#state{event_queue = Neq};
queue_prune(State) ->
    State.

priority_sync(#state{priority = Lp} = State) ->
    Ap = case remote_priority() of
             Lp ->
                 Lp;
             Rp ->
                 % Check once again.
                 Rp2 = remote_priority(),
                 [Sp | _] = most([Lp, Rp, Rp2]),
                 Sp
         end,
    State#state{priority = Ap}.

remote_priority() ->
    Nodes = erlang:nodes(),
    if length(Nodes) == 0 ->
           ?DEFAULT_PRIORITY;
       true ->
           RandNode =
               lists:nth(
                   rand:uniform(length(Nodes)), Nodes),
           gen_server:call({RandNode, ?MODULE}, get_priority, ?GEN_CALL_TIMEOUT)
    end.

most(List) ->
    Lc = lists:foldl(fun(E1, E2) -> maps:put(E1, maps:get(E1, E2, 0) + 1, E2) end, #{}, List),
    Ls = lists:sort(fun({_, Ac}, {_, Bc}) -> Ac > Bc end, maps:to_list(Lc)),
    [Value || {Value, _Count} <- Ls].
