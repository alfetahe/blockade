-module(blockade).

-include("blockade_header.hrl").

%%------------------------------------------------------------------------------
%% Public API exports
%%------------------------------------------------------------------------------
-export([add_handler/2, dispatch/4,
         dispatch_async/4, set_priority/3, set_priority_async/3]).

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

-spec dispatch(event_manager(), event(), event_payload(), dispatch_opts()) ->
                  {ok, event_dispatched} | {ok, event_queued} | {ok, event_discarded}.
dispatch(EventManager, Event, Payload, Opts) ->
    gen_server:call(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)}).

-spec dispatch_async(event_manager(),
                    event(),
                    event_payload(),
                    dispatch_opts()) ->
                       ok.
dispatch_async(EventManager, Event, Payload, Opts) ->
    gen_server:cast(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)}).

-spec set_priority_async(event_manager(), priority(), priority_opts()) -> ok.
set_priority_async(EventManager, Priority, Opts) ->
        gen_server:cast(EventManager, {set_priority, Priority, Opts}).

-spec set_priority(event_manager(), priority(), priority_opts()) -> {ok, priority_set}.
set_priority(EventManager, Priority, Opts) ->
    gen_server:call(EventManager, {set_priority, Priority, Opts}).

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

format_opts(Opts) ->
    Priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY),
    maps:put(priority, Priority, Opts).
