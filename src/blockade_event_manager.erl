-module(blockade_event_manager).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3]).

%%------------------------------------------------------------------------------
%% Record definitions
%%------------------------------------------------------------------------------
-record(state,
        {manager :: blockade:event_manager(),
         event_queue = [] :: list(),
         priority_level = 0 :: integer(),
         schduler_ref = undefined :: reference() | undefined}).

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
    when Priority >= State#state.priority_level ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {noreply, State};
handle_cast({dispatch, Event, Payload, Opts}, State) ->
    {noreply, queue_event(Event, Payload, Opts, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({set_priority, PriorityLvl}, _From, State) ->
    NewState =
        State#state{priority_level = PriorityLvl, schduler_ref = undefined},
    {reply, {ok, priority_set}, NewState};
handle_call({dispatch, Event, Payload, #{priority := Priority} = Opts},
            _From,
            State)
    when Priority >= State#state.priority_level ->
    dispatch_event(Event, Payload, State#state.manager, Opts),
    {reply, {ok, event_dispatched}, State};
handle_call({dispatch, Event, Payload, Opts}, _From, State) ->
    {reply, {ok, event_queued}, queue_event(Event, Payload, Opts, State)};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

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
