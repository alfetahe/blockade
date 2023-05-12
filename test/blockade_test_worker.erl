-module(blockade_test_worker).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

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
