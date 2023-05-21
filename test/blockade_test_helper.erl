-module(blockade_test_helper).

-include("../include/blockade_header.hrl").

-behaviour(gen_server).

-export([start_link/1, stop/1, all_messages/1, add_handler_nodes/3,
         remove_handler_nodes/3, start_pg_nodes/2, get_pids/2, get_all_messages/1,
         test_sync_msg/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%------------------------------------------------------------------------------
%% Helper functions.
%%------------------------------------------------------------------------------
all_messages(Messages) ->
    receive
        Msg ->
            all_messages([Msg | Messages])
    after 0 ->
        Messages
    end.

test_sync_msg(Name, Nodes) ->
    Fun = fun() -> gen_server:call(?PROCESS_NAME(Name, "helper"), test_sync_msg) end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes],
    Fun(),
    ok.

remove_handler_nodes(Man, Event, Nodes) ->
    Fun = fun() -> gen_server:call(?PROCESS_NAME(Man, "helper"), {remove_handler, Man, Event})
          end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes],
    Fun().

add_handler_nodes(Man, Event, Nodes) ->
    Fun = fun() -> gen_server:call(?PROCESS_NAME(Man, "helper"), {add_handler, Man, Event})
          end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes],
    Fun().

start_pg_nodes(Man, Nodes) ->
    Fun = fun() -> pg:start(?PROCESS_NAME(Man, "pg")) end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes].

get_pids(Name, Nodes) ->
    GetPidFun = fun() -> erlang:whereis(?PROCESS_NAME(Name, "helper")) end,
    [erpc:call(Node, GetPidFun) || {_, _, Node} <- Nodes].

get_all_messages(Messages) ->
    receive
        Msg ->
            get_all_messages([Msg | Messages])
    after 0 ->
        Messages
    end.

%%------------------------------------------------------------------------------
%% GenServer functions.
%%------------------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link({local, ?PROCESS_NAME(Name, "helper")}, ?MODULE, [], []).

stop(Name) ->
    gen_server:stop(?PROCESS_NAME(Name, "helper")).

init(Args) ->
    {ok, Args}.

handle_call({remove_handler, EventManager, Event}, _From, State) ->
    blockade:remove_handler(EventManager, Event),
    {reply, ok, State};
handle_call({add_handler, EventManager, Event}, _From, State) ->
    blockade:add_handler(EventManager, Event),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_, {Msg, Pid}}, State) when is_pid(Pid) ->
    Pid ! Msg,
    {noreply, State};
handle_info({_, _EventPayload}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
