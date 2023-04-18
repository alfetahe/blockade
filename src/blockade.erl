-module(blockade).

%%------------------------------------------------------------------------------
%% Public API exports
%%------------------------------------------------------------------------------
-export([add_handler/2, dispatch/3, dispatch_cast/3, dispatch/4,
         dispatch_cast/4, set_priority/2]).

-export_type([event_manager/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type event_manager() :: atom().
-type event() :: atom().
-type event_payload() :: term().
-type priority() :: integer().
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

-spec dispatch(event_manager(),
               event(),
               event_payload(),
               dispatch_opts()) ->
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
    gen_server:call(EventManager, {set_priority, Priority}).

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

format_opts(Opts) ->
    Priority = maps:get(priority, Opts, medium),
    maps:put(priority, Priority, Opts).
