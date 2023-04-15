-module(blockade_event_manager).

-behaviour(gen_statem).

-export([start_link/1]).
-export([init/1, callback_mode/0]).
-export([open/3, locked_low/3, locked_normal/3, locked_high/3, locked_full/3]).

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
    [state_functions, state_enter].

%%------------------------------------------------------------------------------
%% State callbacks
%%------------------------------------------------------------------------------

open(enter, OldState, Data) ->
    ok;
open(EventType, EventContent, Data) ->
    ok.

locked_low(enter, OldState, Data) ->
    ok;
locked_low(EventType, EventContent, Data) ->
    ok.

locked_normal(enter, OldState, Data) ->
    ok;
locked_normal(EventType, EventContent, Data) ->
    ok.

locked_high(enter, OldState, Data) ->
    ok;
locked_high(EventType, EventContent, Data) ->
    ok.

locked_full(enter, OldState, Data) ->
    ok;
locked_full(EventType, EventContent, Data) ->
    ok.
