-module(blockade).

-include("include/blockade_header.hrl").

%%------------------------------------------------------------------------------
%% Public API exports
%%------------------------------------------------------------------------------
-export([child_spec/1, start_link/1, start_link/2, add_handler/2, dispatch/3, dispatch/4, dispatch_sync/3, dispatch_sync/4,
         set_priority/2, set_priority/3, get_priority/1, get_handlers/2, get_events/1,
         remove_handler/2, get_event_queue/1, prune_event_queue/1, discard_events/2,
         local_manager_state/1]).

-export_type([event_manager/0, queued_event/0, priority/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type event_manager() :: atom().
-type event() :: atom().
-type queued_event() :: {event(), event_payload(), dispatch_opts()}.
-type event_payload() :: term().
-type priority() :: integer().
-type discard_events() :: boolean().
-type priority_opts() :: #{reset_after => integer(), discard_events => discard_events()}.
-type dispatch_opts() :: #{priority => priority(), members => local | global}.
-type start_up_opts() :: #{priority => priority(), discard_events => discard_events()}.

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------
-spec start_link(event_manager()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Name) ->
    blockade_sup:start_link(Name).

-spec start_link(event_manager(), start_up_opts()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Name, Opts) ->
    blockade_sup:start_link(Name, Opts).

child_spec(#{name := Name} = opts) ->
    #{id => ?PROCESS_NAME(Name, "sup"),
      start => {blockade_sup, start_link, [opts]},
      type => worker,
      restart => permanent,
      shutdown => 500}.

-spec add_handler(event_manager(), event()) -> ok.
add_handler(EventManager, Event) ->
    pg:join(?PROCESS_NAME(EventManager, "pg"), Event, self()).

-spec remove_handler(event_manager(), event()) -> ok | not_joined.
remove_handler(EventManager, Event) ->
    pg:leave(?PROCESS_NAME(EventManager, "pg"), Event, self()).

-spec get_handlers(event_manager(), event()) -> {ok, [pid()]}.
get_handlers(EventManager, Event) ->
    {ok, pg:get_members(?PROCESS_NAME(EventManager, "pg"), Event)}.

-spec get_events(event_manager()) -> {ok, [event()]}.
get_events(EventManager) ->
    {ok, pg:which_groups(?PROCESS_NAME(EventManager, "pg"))}.

-spec dispatch(event_manager(), event(), event_payload()) -> ok.
dispatch(EventManager, Event, Payload) ->
    dispatch(EventManager, Event, Payload, #{}).

-spec dispatch(event_manager(), event(), event_payload(), dispatch_opts()) -> ok.
dispatch(EventManager, Event, Payload, Opts) ->
    gen_server:cast(EventManager, {dispatch, Event, Payload, format_opts(Opts)}).

-spec dispatch_sync(event_manager(), event(), event_payload()) ->
                       {ok, event_dispatched} | {ok, event_queued} | {ok, event_discarded}.
dispatch_sync(EventManager, Event, Payload) ->
    dispatch_sync(EventManager, Event, Payload, #{}).

-spec dispatch_sync(event_manager(), event(), event_payload(), dispatch_opts()) ->
                       {ok, event_dispatched} | {ok, event_queued} | {ok, event_discarded}.
dispatch_sync(EventManager, Event, Payload, Opts) ->
    gen_server:call(EventManager,
                    {dispatch, Event, Payload, format_opts(Opts)},
                    ?GEN_CALL_TIMEOUT).

-spec set_priority(event_manager(), priority()) -> ok | {error, priority_not_integer}.
set_priority(EventManager, Priority) ->
    set_priority(EventManager, Priority, #{}).

-spec set_priority(event_manager(), priority(), priority_opts()) ->
                      ok | {error, priority_not_integer}.
set_priority(EventManager, Priority, Opts) when is_integer(Priority) ->
    gen_server:abcast(get_nodes(), EventManager, {set_priority, Priority, Opts}),
    ok;
set_priority(_, _, _) ->
    {error, priority_not_integer}.

-spec get_priority(event_manager()) -> {ok, priority()}.
get_priority(EventManager) ->
    gen_server:call(EventManager, get_priority).

-spec get_event_queue(event_manager()) -> {ok, list()}.
get_event_queue(EventManager) ->
    gen_server:call(EventManager, get_event_queue).

-spec prune_event_queue(event_manager()) -> ok.
prune_event_queue(EventManager) ->
    gen_server:abcast(get_nodes(), EventManager, prune_event_queue),
    ok.

-spec discard_events(event_manager(), discard_events()) -> ok | {error, flag_not_boolean}.
discard_events(EventManager, Flag) when is_boolean(Flag) ->
    gen_server:abcast(get_nodes(), EventManager, {discard_events, Flag}),
    ok;
discard_events(_EventManager, _Flag) ->
    {error, flag_not_boolean}.

-spec local_manager_state(event_manager()) -> {ok, map()}.
local_manager_state(EventManager) ->
    gen_server:call(EventManager, get_state).

%%--------------------------------------------------------------------------------------------------
%% Private functions
%%--------------------------------------------------------------------------------------------------

format_opts(Opts) ->
    Priority = maps:get(priority, Opts, ?DEFAULT_PRIORITY),
    maps:put(priority, Priority, Opts).

get_nodes() ->
    [node() | erlang:nodes([visible])].
