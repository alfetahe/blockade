-module(blockade_test_helper).

-include("../include/blockade_header.hrl").

-behaviour(gen_server).

-export([start_link/1, all_messages/1, add_handler_nodes/3, remove_handler_nodes/3,
         start_pg_nodes/2, get_pids/1, get_all_messages/1, test_sync_msg/1]).
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

test_sync_msg(Nodes) ->
    Fun = fun() -> gen_server:call(blockade_test_helper, test_sync_msg) end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes],
    Fun(),
    ok.

remove_handler_nodes(Man, Event, Nodes) ->
    Fun = fun() -> gen_server:call(blockade_test_helper, {remove_handler, Man, Event}) end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes],
    Fun().

add_handler_nodes(Man, Event, Nodes) ->
    Fun = fun() -> gen_server:call(blockade_test_helper, {add_handler, Man, Event}) end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes],
    Fun().

start_pg_nodes(Man, Nodes) ->
    Fun = fun() -> pg:start(?PROCESS_NAME(Man, "pg")) end,
    [erpc:call(Node, Fun) || {_, _, Node} <- Nodes].

get_pids(Nodes) ->
    GetPidFun = fun() -> erlang:whereis(?MODULE) end,
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
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

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

handle_info({test_event, {Msg, Pid}}, State) when is_pid(Pid) ->
    Pid ! Msg,
    {noreply, State};
handle_info({test_event, _EventPayload}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
