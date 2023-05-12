-module(blockade_test_helper).

-behaviour(gen_server).

-export([start_link/1, all_messages/1, add_handler_nodes/3]).
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

add_handler_nodes(EventManager, Event, Nodes) ->
    [erpc:call(Node,
               fun() -> gen_server:call(blockade_test_worker, {add_handler, EventManager, Event})
               end)
     || {_, _, Node} <- Nodes],
    blockade:add_handler(EventManager, Event).

%%------------------------------------------------------------------------------
%% GenServer functions.
%%------------------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    {ok, Args}.

handle_call({add_handler, EventManager, Event}, _From, State) ->
    blockade:add_handler(EventManager, Event),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({test_event, _EventPayload}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
