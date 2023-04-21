-module(blockade).

-include("blockade_header.hrl").

%%------------------------------------------------------------------------------
%% Public API exports
%%------------------------------------------------------------------------------
-export([add_handler/2, dispatch/4, dispatch_sync/4, set_priority/3,
         get_priority/1]).

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
-type dispatch_opts() ::
    #{priority => priority(),
      members => local | global,
      timeout => integer()}.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------
-spec add_handler(event_manager(), event()) -> ok.
add_handler(EventManager, Event) ->
    pg:join(EventManager, Event, self()).

-spec dispatch(event_manager(), event(), event_payload(), dispatch_opts()) ->
                  {ok, event_dispatched} |
                  {ok, event_queued} |
                  {ok, event_discarded}.
dispatch(EventManager, Event, Payload, Opts) ->
    gen_server:cast(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)}).

-spec dispatch_sync(event_manager(),
                    event(),
                    event_payload(),
                    dispatch_opts()) ->
                       ok.
dispatch_sync(EventManager, Event, Payload, Opts) ->
    gen_server:call(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)},
                    maps:get(timeout, Opts, ?GEN_CALL_TIMEOUT)).

-spec set_priority(event_manager(), priority(), priority_opts()) -> ok.
set_priority(EventManager, Priority, Opts) ->
    Nodes = [node() | erlang:nodes([visible])],
    gen_server:abcast(Nodes, EventManager, {set_priority, Priority, Opts}).

-spec get_priority(event_manager()) -> {ok, priority()}.
get_priority(EventManager) ->
    gen_server:call(EventManager, get_priority).

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

format_opts(Opts) ->
    Priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY),
    maps:put(priority, Priority, Opts).
