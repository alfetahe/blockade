-module(blockade_event_manager).

-behaviour(gen_statem).

-export([start_link/1]).
-export([init/1, callback_mode/0]).
-export([handle_event/4]).

%%------------------------------------------------------------------------------
%% Record definitions
%%------------------------------------------------------------------------------
-record(state,
        {manager :: blockade:event_manager(),
         event_queue = [] :: list(),
         opts = #{} :: map()}).

%%------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------
start_link(#{name := Name} = _Args) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Name], []).

%%------------------------------------------------------------------------------
%% Behaviour required callbacks
%%------------------------------------------------------------------------------
init(Name) ->
    {ok, low, #state{manager = Name}}.

callback_mode() ->
    [handle_event_function, state_enter].

%%------------------------------------------------------------------------------
%% State callbacks
%%------------------------------------------------------------------------------

% TODO: use pattern matching and guards to handle the events.
handle_event({call, From}, {set_priority, NewPriority}, _PriorityLvl, Data) ->
    {next_state, NewPriority, Data, [{reply, From, ok}]};
handle_event({call, From},
             {dispatch, EventKey, Payload, #{priority := Priority} = Opts},
             PriorityLvl,
             Data) ->
    StateRes =
        case maps:get(new_priority, Opts, PriorityLvl) of
            PriorityLvl ->
                [keep_state];
            NewState ->
                [next_state, NewState]
        end,

    NewData =  
        case priority_validation(Priority, PriorityLvl) of
            true ->
                dispatch_event(EventKey, Payload, Data#state.manager, Opts),
                Data;
            false ->
                queue_event(EventKey, Payload, Opts, Data)
        end,

    Actions =    
        case priority_validation(Priority, PriorityLvl) of
            true ->
                [[{reply, From, event_dispatched}]];
            false ->
                [[{reply, From, {ok, event_queued}}]]
        end,

    list_to_tuple(StateRes ++ NewData ++ Actions);
handle_event(enter, _OldState, _PriorityLvl, Data) ->
    {keep_state, Data};
handle_event(_EventType, _EventContent, _PriorityLvl, Data) ->
    {keep_state, Data}.

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

dispatch_event(EventKey, Payload, Manager, Opts) ->
    Members = maps:get(members, Opts, global),

    Pids =
        case Members of
            local ->
                pg:get_local_members(Manager);
            global ->
                pg:get_members(Manager);
            _ ->
                throw({error, invalid_members_option})
        end,
    send_messages(Pids, EventKey, Payload).

send_messages([], _EventKey, _Payload) ->
    ok;
send_messages([Pid | Pids], EventKey, Payload) ->
    Pid ! {EventKey, Payload},
    send_messages(Pids, EventKey, Payload).

queue_event(EventKey, Payload, Opts, Data) ->
    NewQueue = [{EventKey, Payload, Opts} | Data#state.event_queue],
    Data#state{event_queue = NewQueue}.

priority_mapping(Priority) ->
    case Priority of
        low ->
            1;
        medium ->
            2;
        high ->
            3;
        critical ->
            4;
        true ->
            throw({error, invalid_priority})
    end.

priority_validation(PassedPriority, PriorityLvl) ->
    priority_mapping(PassedPriority) >= priority_mapping(PriorityLvl).
