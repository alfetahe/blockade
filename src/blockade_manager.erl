-module(blockade_manager).

-behaviour(gen_statem).

-export([start_link/1]).
-export([handle_event/4]).
-export([init/1, callback_mode/0]).

-record(state_data, {event_subscibers, events_emitted}).

start_link(#{name := Name} = _Args) ->
    gen_statem:start_link({local, Name}, ?MODULE, [], []).

init(_) ->
    StateData = #state_data{event_subscibers = #{}, events_emitted = []},
    {ok, idle, StateData}.

callback_mode() ->
    [handle_event_function].

%%% State callbacks

handle_event({call, From}, {add_event_subscriber, EventKey, Pid, Opts}, _State, StateData) ->
    NewStateData = StateData#state_data{event_subscibers = maps:put(EventKey, {Pid, Opts}, StateData#state_data.event_subscibers)},
    {keep_state, NewStateData, {reply, From, ok}};
handle_event({call, From}, {dispatch_event, _EventKey, _EventData, _Opts}, idle, StateData) ->
    {next_state, dispatching_event, StateData, {reply, From, ok}};
handle_event({call, From}, _EventData, _State, StateData) ->
    {keep_state, StateData, {reply, From, {error, unknown_message}}};
handle_event(_EventType, _EventData, _State, StateData) ->
    {keep_state, StateData}.