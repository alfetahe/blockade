-module(blockade_manager).

-behaviour(gen_statem).

-export([start_link/1]).
-export([wait_event/3]).
-export([init/1, callback_mode/0]).

-record(state_data, {event_subscibers,events_emitted}).

start_link(_) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, #state_data{event_subscibers = #{}, events_emitted = []}).

init(StateData) ->
    {ok, wait_event, StateData}.

callback_mode() ->
    [state_functions,state_enter].

%%% State callbacks

wait_event(_EventType, _EventContent, StateData) ->
    {keep_state, StateData}.
