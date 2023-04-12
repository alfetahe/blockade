-module(blockade_manager).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([event_key/0, event_data/0, sub_opts/0]).

%-------------------------------------------------------------------------------
% Types
%-------------------------------------------------------------------------------
-type event_key() :: atom().
-type event_data() :: term().
-type sub_opts() :: #{handler_type => normal | gen_cast | gen_call}.
-type event_sub_opts() :: #{pid() => sub_opts()}.
-type subs_data() :: #{event_key() => event_sub_opts()}.

%-------------------------------------------------------------------------------
% Record definitions
%-------------------------------------------------------------------------------
-record(data,
        {subs_data :: subs_data(),
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
-spec init([]) -> {ok, #data{}}.
init(_) ->
    {ok, _Pid} = pg:start_link(),
    {ok, #data{subs_data = #{}, event_queue = []}}.

handle_call({add_sub, EventKey, Pid, Opts}, _From, State) ->
    pg:join(EventKey, Pid),
    SubOpts = maps:get(EventKey, State#data.subs_data, #{}),
    NewSubOpts = maps:put(Pid, Opts, SubOpts),
    NewState =
        State#data{subs_data =
                       maps:put(EventKey, NewSubOpts, State#data.subs_data)},
    {reply, ok, NewState};
handle_call({dispatch, EventKey, EventData}, _From, State) ->
    blockade_executor:execute_callbacks(
        pg:get_members(EventKey),
        EventKey,
        EventData,
        maps:get(EventKey, State#data.subs_data, #{})),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.
