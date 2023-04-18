-module(blockade).

-include("blockade_header.hrl").

%%------------------------------------------------------------------------------
%% Public API exports
%%------------------------------------------------------------------------------
-export([add_handler/2, dispatch/3, dispatch_cast/3, dispatch/4,
         dispatch_cast/4, set_priority/2, set_priority/3]).

-export_type([event_manager/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type event_manager() :: atom().
-type event() :: atom().
-type event_payload() :: term().
-type priority() :: integer().
-type priority_opts() ::
    #{reset_after => integer(), discard_events => boolean()}.
-type dispatch_opts() :: #{priority => priority(), members => local | global}.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------
-spec add_handler(event_manager(), event()) -> ok.
add_handler(EventManager, Event) ->
    pg:join(EventManager, Event, self()).

-spec dispatch(event_manager(), event(), event_payload()) -> ok.
dispatch(EventManager, Event, Payload) ->
    dispatch(EventManager, Event, Payload, #{priority => medium}).

-spec dispatch_cast(event_manager(), event(), event_payload()) -> ok.
dispatch_cast(EventManager, Event, Payload) ->
    dispatch_cast(EventManager, Event, Payload, #{priority => medium}).

-spec dispatch(event_manager(), event(), event_payload(), dispatch_opts()) ->
                  ok.
dispatch(EventManager, Event, Payload, Opts) ->
    gen_server:call(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)}).

-spec dispatch_cast(event_manager(),
                    event(),
                    event_payload(),
                    dispatch_opts()) ->
                       ok.
dispatch_cast(EventManager, Event, Payload, Opts) ->
    gen_server:cast(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)}).

-spec set_priority(event_manager(), priority()) -> ok.
set_priority(EventManager, Priority) ->
    set_priority(EventManager, Priority, #{}).

-spec set_priority(event_manager(), priority(), priority_opts()) -> ok.
set_priority(EventManager, Priority, Opts) ->
    gen_server:call(EventManager, {set_priority, Priority, Opts}).

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

format_opts(Opts) ->
    Priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY),
    maps:put(priority, Priority, Opts).
