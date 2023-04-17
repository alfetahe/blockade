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
-type event_key() :: atom().
-type event_payload() :: term().
-type priority() :: low | medium | high | critical.
-type dispatch_opts() ::
    #{priority => priority(),
      members => local | global,
      new_priority => priority()}.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------
-spec add_handler(event_manager(), event_key()) -> ok.
add_handler(EventManager, EventKey) ->
    pg:join(EventManager, EventKey, self()).

-spec dispatch(event_manager(), event_key(), event_payload()) -> ok.
dispatch(EventManager, EventKey, Payload) ->
    dispatch(EventManager, EventKey, Payload, #{priority => medium}).

-spec dispatch_cast(event_manager(), event_key(), event_payload()) -> ok.
dispatch_cast(EventManager, EventKey, Payload) ->
    dispatch_cast(EventManager, EventKey, Payload, #{priority => medium}).

-spec dispatch(event_manager(),
               event_key(),
               event_payload(),
               dispatch_opts()) ->
                  ok.
dispatch(EventManager, EventKey, Payload, Opts) ->
    gen_statem:call(EventManager,
                    {dispatch, EventKey, Payload, format_opts(Opts)}).

-spec dispatch_cast(event_manager(),
                    event_key(),
                    event_payload(),
                    dispatch_opts()) ->
                       ok.
dispatch_cast(EventManager, EventKey, Payload, Opts) ->
    gen_statem:cast(EventManager,
                    {dispatch, EventKey, Payload, format_opts(Opts)}).

-spec set_priority(event_manager(), priority()) -> ok.
set_priority(EventManager, Priority) ->
    gen_statem:call(EventManager, {set_priority, Priority}).

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

format_opts(Opts) ->
    Priority = maps:get(priority, Opts, medium),
    maps:put(priority, Priority, Opts).
