-module(blockade_manager).

-behaviour(gen_statem).

-export([start_link/1]).
-export([idle/3]).
-export([init/1, callback_mode/0]).

-record(state_data, {event_subscibers, events_emitted}).

start_link(#{name := Name} = _Args) ->
    gen_statem:start_link({local, Name}, ?MODULE, [], []).

init(_) ->
    StateData = #state_data{event_subscibers = #{}, events_emitted = []},
    {ok, idle, StateData}.

callback_mode() ->
    [state_functions, state_enter].

%%% State callbacks

idle({call, From}, {add_event_subscriber, EventKey, Pid, Opts}, StateData) ->
    NewStateData = StateData#state_data{event_subscibers = maps:put(EventKey, {Pid, Opts}, StateData#state_data.event_subscibers)},
    {keep_state, NewStateData, {reply, From, ok}};
idle(_EventType, _EventData, StateData) ->
    {keep_state, StateData}.
