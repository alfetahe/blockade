-module(blockade_overviewer).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    monitor_manager_singleton(),
    {ok, Args}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, {blockade_manager, _Node}, _Info},
            State) ->
    blockade_sup:start_manager(State),
    monitor_manager_singleton(),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

monitor_manager_singleton() ->
    monitor(process, blockade_manager).
