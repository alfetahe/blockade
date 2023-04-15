-module(blockade).

%%------------------------------------------------------------------------------
%% Public API exports
%%------------------------------------------------------------------------------
-export([add_handler/2, dispatch/3, dispatch_cast/3, dispatch/4,
         dispatch_cast/4, lock_manager/2, unlock_manager/1]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type event_manager() :: atom().
-type event_key() :: atom().
-type event_payload() :: term().
-type lock_level() :: low | normal | high | full.
-type dispatch_opts() :: #{}.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------
-spec add_handler(event_manager(), event_key()) -> ok.
add_handler(EventManager, EventKey) ->
    pg:join(EventManager, EventKey, self()).

-spec dispatch(event_manager(), event_key(), event_payload()) -> ok.
dispatch(EventManager, EventKey, Payload) ->
    dispatch(EventManager, EventKey, Payload, #{}).

-spec dispatch_cast(event_manager(), event_key(), event_payload()) -> ok.
dispatch_cast(EventManager, EventKey, Payload) ->
    dispatch_cast(EventManager, EventKey, Payload, #{}).

-spec dispatch(event_manager(),
               event_key(),
               event_payload(),
               dispatch_opts()) ->
                  ok.
dispatch(EventManager, EventKey, Payload, Opts) ->
    gen_statem:call(EventManager, {dispatch, EventKey, Payload, Opts}).

-spec dispatch_cast(event_manager(),
                    event_key(),
                    event_payload(),
                    dispatch_opts()) ->
                       ok.
dispatch_cast(EventManager, EventKey, Payload, Opts) ->
    gen_statem:cast(EventManager, {dispatch, EventKey, Payload, Opts}).

-spec lock_manager(event_manager(), lock_level()) -> ok.
lock_manager(EventManager, LockLevel) ->
    gen_statem:call(EventManager, {lock, LockLevel}).

-spec unlock_manager(event_manager()) -> ok.
unlock_manager(EventManager) ->
    gen_statem:call(EventManager, unlock).
