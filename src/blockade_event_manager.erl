-module(blockade_event_manager).

-behaviour(gen_statem).

-export([start_link/1]).
-export([init/1, callback_mode/0]).
-export([handle_event/4]).

%%------------------------------------------------------------------------------
%% Record definitions
%%------------------------------------------------------------------------------
-record(state, {event_queue = [] :: list(), opts = #{} :: map()}).

%%------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------
start_link(#{name := Name} = _Args) ->
    gen_statem:start_link({local, Name}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% Behaviour required callbacks
%%------------------------------------------------------------------------------
init(_Opts) ->
    {ok, available, #state{}}.

callback_mode() ->
    [handle_event_function, state_enter].

%%------------------------------------------------------------------------------
%% State callbacks
%%------------------------------------------------------------------------------

% TODO: use pattern matching and guards to handle the events.
handle_event(enter, OldState, State, Data) ->
    {next_state, OldState, State, Data};

handle_event(EventType, EventContent, State, Data) ->
    {next_state, State, State, Data}. 
