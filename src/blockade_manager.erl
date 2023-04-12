-module(blockade_manager).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([event_key/0, event_data/0, subscriber_opts/0]).

%-------------------------------------------------------------------------------
% Types
%-------------------------------------------------------------------------------
-type event_key() :: atom().
-type event_data() :: term().
-type subscriber_opts() :: #{handler_type => normal | gen_cast | gen_call}.
-type event_subscriber() :: {pid(), subscriber_opts()}.
-type event_subscribers() :: [event_subscriber()].
-type events() :: #{event_key() => event_subscribers()}.

%-------------------------------------------------------------------------------
% Record definitions
%-------------------------------------------------------------------------------
-record(data,
        {subscribers :: events(),
         event_queue :: [{event_key(), event_data()}]}).

%-------------------------------------------------------------------------------
% Internal API
%-------------------------------------------------------------------------------
start_link(#{name := Name} = Args) ->
    case gen_server:start_link({global, Name}, ?MODULE, Args, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Else ->
            Else
    end.

%-------------------------------------------------------------------------------
% Genserver callbacks
%-------------------------------------------------------------------------------
-spec init([]) -> {ok, idle, events()}.
init(_) ->
    {ok, idle, #data{subscribers = #{}, event_queue = []}}.

handle_call({add_sub, EventKey, Pid, Opts}, _From, State) ->
    EventSubscribers = maps:get(EventKey, State#data.subscribers, []),
    NewState =
        State#data{subscribers =
                       maps:put(EventKey,
                                [{Pid, Opts} | EventSubscribers],
                                State#data.subscribers)},
    {reply, ok, NewState};
handle_call({dispatch, EventKey, EventData}, _From, State) ->
    blockade_executor:execute_callbacks(
        maps:get(EventKey, State#data.subscribers, []), EventKey, EventData),
    % NewState =
    %     State#data{event_queue =
    %                    [{EventKey, EventData} | State#data.event_queue]},
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.
