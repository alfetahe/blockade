-module(blockade).

-export([subscribe/3, dispatch/3]).

%-------------------------------------------------------------------------------
% Types
%-------------------------------------------------------------------------------
-type event_manager() :: pid() | atom().

%-------------------------------------------------------------------------------
% Public API
%-------------------------------------------------------------------------------
-spec subscribe(event_manager(),
                blockade_manager:event_key(),
                blockade_manager:subscriber_opts()) ->
                   ok.
subscribe(EventManager, EventKey, Opts) ->
    gen_server:call(EventManager, {add_sub, EventKey, self(), Opts}).

-spec dispatch(event_manager(),
               blockade_manager:event_key(),
               blockade_manager:event_data()) ->
                  ok.
dispatch(EventManager, EventKey, EventData) ->
    gen_server:call(EventManager, {dispatch, EventKey, EventData}).
