-module(blockade_manager).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type event_key() :: atom().
-type event_data() :: term().
-type event_subscriber() :: {pid(), list()}.
-type event_subscribers() :: [event_subscriber()].
-type events() :: #{event_key() => event_subscribers()}.

-record(data,
        {events :: events(),
         waiting_caller :: pid(),
         dispatched_event :: {event_key(), event_data()}}).

start_link(#{name := Name} = Args) ->
    case gen_server:start_link({global, Name}, ?MODULE, Args, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Else ->
            Else
    end.

-spec init([]) -> {ok, idle, events()}.
init(_) ->
    {ok,
     idle,
     #data{events = #{},
           waiting_caller = undefined,
           dispatched_event = undefined}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

% callback_mode() ->
%     [state_functions, state_enter].

% idle({call, From}, {add_sub, EventKey, Pid, Opts}, LoopData) ->
%     EventSubscribers = maps:get(EventKey, LoopData#data.events, []),
%     NewLoopData =
%         LoopData#data{events =
%                           maps:put(EventKey,
%                                    [{Pid, Opts} | EventSubscribers],
%                                    LoopData#data.events)},
%     {keep_state, NewLoopData, {reply, From, ok}};
% idle({call, From}, {dispatch, EventKey, EventData}, LoopData) ->
%     NewLoopData =
%         LoopData#data{waiting_caller = From, dispatched_event = {EventKey, EventData}},
%     {next_state, dispatch, NewLoopData};
% idle(enter, _OldState, LoopData) ->
%     io:format("Idle state entered~n"),
%     {keep_state, LoopData};
% idle(Any, _OldState, LoopData) ->
%     io:format("Any hiti~n"),
%     {keep_state, LoopData}.

% idle(Any, _OldState, LoopData, Mpre) ->
%     io:format("Any hiti~n"),
%     {keep_state, LoopData}.

% dispatch(enter, _OldState, LoopData) ->
%     {EventKey, EventData} = LoopData#data.dispatched_event,
%     EventSubscribers = maps:get(EventKey, LoopData#data.events, []),
%     call_subscribers(EventSubscribers, EventData),
%     gen_statem:reply(LoopData#data.waiting_caller, ok),
%     NewLoopData = LoopData#data{waiting_caller = undefined, dispatched_event = undefined},
%     {next_state, idle, LoopData}.

% %%% State callbacks

% % handle_event({call, From}, {add_sub, EventKey, Pid, Opts}, idle, Events) ->
% %     {keep_state, add_sub(Events, EventKey, {Pid, Opts}), {reply, From, ok}};
% % handle_event({call, From}, {dispatch, EventKey, EventData}, idle, Events) ->
% %     #{EventKey := Subscribers} = Events,
% %     call_subscribers(Subscribers, EventData),
% %     {next_state, idle, Events};
% % handle_event({call, From}, _EventData, _State, Events) ->
% %     {keep_state, Events, {reply, From, {error, unknown_message}}};
% % handle_event(_EventType, _EventData, _State, Events) ->
% %     {keep_state, Events}.

% % %%% Internal functions

% % add_sub(Events, EventKey, Subscriber) ->
% %     Subscribers = maps:get(EventKey, Events, []),
% %     maps:put(EventKey, [Subscriber | Subscribers], Events).

% call_subscribers([Subscriber | Subscribers], EventData) ->
%     call_subscriber(Subscriber, EventData),
%     call_subscribers(Subscribers, EventData);
% call_subscribers([], _EventData) ->
%     ok.

% call_subscriber({Pid, Opts}, _EventData) ->
%     timer:sleep(3000),
%     io:format("Calling ~p with ~p~n", [Pid, Opts]).    %Pid ! {event, Opts}.
