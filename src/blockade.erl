%%%-----------------------------------------------------------------------------
%% @doc This module defines the public API for the blockade application.
%%
%%
%%
%% @end
%%%-----------------------------------------------------------------------------
-module(blockade).

-include("blockade_header.hrl").

%%------------------------------------------------------------------------------
%% Public API exports
%%------------------------------------------------------------------------------
-export([child_spec/1, start_link/1, add_handler/2, dispatch/3, dispatch/4,
         dispatch_sync/3, dispatch_sync/4, set_priority/2, set_priority/3, get_priority/1,
         get_handlers/2, get_events/1, remove_handler/2, get_event_queue/1, prune_event_queue/1,
         discard_events/2, local_manager_state/1]).

%%------------------------------------------------------------------------------
%% Exported types
%%------------------------------------------------------------------------------
-export_type([event_manager/0, queued_event/0, priority/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

%% The name of the event manager process.
-type event_manager() :: atom().
%% The name of the event.
-type event() :: atom().
%% Event payload which can be any term.
-type event_payload() :: term().
%% Boolean indicating whether the priority should be synchronized within the cluster.
-type priority_sync() :: true | false.
%% Priority level. The higher the priority the more important the event.
-type priority() :: integer().
%% Boolean indicating whether to discard events or not when the priority of the event
%%  is lower than the priority of the event manager.
-type event_discard() :: boolean().
%% Boolean indicating whether to discard events or not in case the priority of
%% the event is lower than the priority of the event manager.
-type priority_opts() ::
    #{reset_after => integer(), discard_events => event_discard(),
      local_priority_set => boolean()}.
%% Priority options.
-type start_up_opts() ::
    #{name => event_manager(), priority => priority(), discard_events => event_discard(),
      priority_sync => priority_sync()}.
%% Start up options which can be passed to the start_link function.
-type dispatch_opts() ::
    #{priority => priority(),
      members => local | global | external | [node()],
      discard_event => event_discard(),
      atomic_priority_set => priority(),
      local_priority_set => boolean()}.
%% Dispatch options.
-type queued_event() :: {event(), event_payload(), dispatch_opts()}. %% Queued event.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------

%%% @doc Start a blockade event manager supervisor.
-spec start_link(start_up_opts()) -> {ok, pid()} | ignore | {error, term()}.
start_link(#{name := Name} = Opts) ->
    blockade_sup:start_link(Name, Opts);
start_link(_) ->
    throw(mandatory_option_name_missing).

%% @doc Child spec for the blockade event manager supervisor.
-spec child_spec(map()) -> map().
child_spec(#{name := Name} = Opts) ->
    #{id => ?PROCESS_NAME(Name, "sup"),
      start => {blockade_sup, start_link, [Name, Opts]},
      type => worker,
      restart => permanent,
      shutdown => 5000}.

%% @doc Adds a handler to an event. The process calling the function will be
%% added to the event group.
%%
%% Calling this function multiple times with the same event will add the
%% process to the event group multiple times and the process will receive
%%  the event multiple times.
-spec add_handler(event_manager(), event()) -> ok.
add_handler(EventManager, Event) ->
    pg:join(?PROCESS_NAME(EventManager, "pg"), Event, self()).

%% @doc Remove a handler from an event.
%%
%% The process calling the function will be removed from the event group.
-spec remove_handler(event_manager(), event()) -> ok | not_joined.
remove_handler(EventManager, Event) ->
    pg:leave(?PROCESS_NAME(EventManager, "pg"), Event, self()).

%% @doc Returns a list of process identifiers that are subscribed to the event.
-spec get_handlers(event_manager(), event()) -> {ok, [pid()]}.
get_handlers(EventManager, Event) ->
    {ok, pg:get_members(?PROCESS_NAME(EventManager, "pg"), Event)}.

%% @doc Returns a list of events registered under the event manager.
-spec get_events(event_manager()) -> {ok, [event()]}.
get_events(EventManager) ->
    {ok, pg:which_groups(?PROCESS_NAME(EventManager, "pg"))}.

%% @doc Dispatches event asynchronously.
%%
%% The event will be dispatched to all the processes that are subscribed to
%% the event.
%%
%% If no options are passed to the function, the event will be dispatched
%% across the cluster.
-spec dispatch(event_manager(), event(), event_payload()) -> ok.
dispatch(EventManager, Event, Payload) ->
    dispatch(EventManager, Event, Payload, #{}).

%% @doc Dispatches event asynchronously with options.
%%
%% The event will be dispatched to all the processes that are subscribed to
%% the event.
-spec dispatch(event_manager(), event(), event_payload(), dispatch_opts()) -> ok.
dispatch(EventManager, Event, Payload, Opts) ->
    gen_server:cast(EventManager, {dispatch, Event, Payload, format_opts(Opts)}).

%% @doc Dispatches event synchronously.
%%
%% The function will return when all messages are sent to the subscribed
%% processes. This does not mean that all processes have handled the event
%% because the messages are sent asynchronously.
%%
%% The event will be dispatched to all the processes that are subscribed to
%% the event.
%%
%% If no options are passed to the function, the event will be dispatched
%% across the cluster.
-spec dispatch_sync(event_manager(), event(), event_payload()) ->
                       {ok, event_dispatched} | {ok, event_queued} | {ok, event_discarded}.
dispatch_sync(EventManager, Event, Payload) ->
    dispatch_sync(EventManager, Event, Payload, #{}).

%% @doc Dispatches event synchronously with options.
%%
%% The function will return when all messages are sent to the subscribed
%% processes. This does not mean that all processes have handled the event
%%  because the messages are sent asynchronously.
%%
%% The event will be dispatched to all the processes that are subscribed to
%% the event.
%%
%% If no options are passed to the function, the event will be dispatched
%% across the cluster.
-spec dispatch_sync(event_manager(), event(), event_payload(), dispatch_opts()) ->
                       {ok, event_dispatched} | {ok, event_queued} | {ok, event_discarded}.
dispatch_sync(EventManager, Event, Payload, Opts) ->
    gen_server:call(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)},
                    ?GEN_CALL_TIMEOUT).

%% @doc Sets the priority of the event queue.
%%
%% The priority will be propagated to all the nodes in the cluster.
%%
%% Default priority is `0'.
-spec set_priority(event_manager(), priority()) -> ok | {error, priority_not_integer}.
set_priority(EventManager, Priority) ->
    set_priority(EventManager, Priority, #{}).

%% @doc Sets the priority of the event queue with options.
%%
%% The priority will be propagated to all the nodes in the cluster.
%%
%% Optionally the priority can be reset after a certain number of milliseconds
%% and events can be discarded if the priority of the event is lower than the
%% priority of the event queue.
%%
%% Default priority is `0'.
-spec set_priority(event_manager(), priority(), priority_opts()) ->
                      ok | {error, priority_not_integer}.
set_priority(EventManager, Priority, #{local_priority_set := true} = Opts)
    when is_integer(Priority) ->
    gen_server:cast(EventManager, {set_priority, Priority, Opts}),
    ok;
set_priority(EventManager, Priority, Opts) when is_integer(Priority) ->
    gen_server:abcast(get_nodes(), EventManager, {set_priority, Priority, Opts}),
    ok;
set_priority(_, _, _) ->
    {error, priority_not_integer}.

%% @doc Returns the priority of the event queue.
-spec get_priority(event_manager()) -> {ok, priority()}.
get_priority(EventManager) ->
    gen_server:call(EventManager, get_priority).

%% @doc Returns the list of queued events.
-spec get_event_queue(event_manager()) -> {ok, list()}.
get_event_queue(EventManager) ->
    gen_server:call(EventManager, get_event_queue).

%% @doc Removes all queued events.
-spec prune_event_queue(event_manager()) -> ok.
prune_event_queue(EventManager) ->
    gen_server:abcast(get_nodes(), EventManager, prune_event_queue),
    ok.

%% @doc Boolean flag to enable or disable the event discarding feature.
%%
%% If the flag is set to `true', the event queue will discard events that have
%% lower priority than the event queue priority.
-spec discard_events(event_manager(), event_discard()) -> ok | {error, flag_not_boolean}.
discard_events(EventManager, Flag) when is_boolean(Flag) ->
    gen_server:abcast(get_nodes(), EventManager, {discard_events, Flag}),
    ok;
discard_events(_EventManager, _Flag) ->
    {error, flag_not_boolean}.

%% @doc Returns the state of the event manager.
-spec local_manager_state(event_manager()) -> {ok, map()}.
local_manager_state(EventManager) ->
    gen_server:call(EventManager, get_state).

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

format_opts(Opts) ->
    Priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY),
    maps:put(priority, Priority, Opts).

get_nodes() ->
    [node() | erlang:nodes([visible])].
