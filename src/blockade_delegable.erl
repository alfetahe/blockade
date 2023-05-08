-module(blockade_delegable).

-include("blockade_header.hrl").

-define(SUFFIX, "delegable").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, handle_continue/2]).

start_link(#{name := Name} = Args) ->
    gen_server:start_link({local, ?PROCESS_NAME(Name, ?SUFFIX)}, ?MODULE, Args, []).

init(Args) ->
    erlang:send_after(?PRIORITY_SYNC, self(), priority_sync),
    {ok, Args, {continue, setup}}.

handle_continue(priority_init, #{name := Manager} = State) ->
    Lp = gen_server:call(Manager, get_priority),
    Np = blockade_service:get_sync_priority(Lp, Manager),
    gen_server:cast(Manager, {set_priority, Np, #{keep_old_settings => true}}),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_info(priority_sync, #{name := Manager} = State) ->
    erlang:send_after(?PRIORITY_SYNC, self(), priority_sync),
    Lp = gen_server:call(Manager, get_priority),
    Np = blockade_service:get_sync_priority(Lp, Manager),
    gen_server:cast(Manager, {set_priority, Np, #{keep_old_settings => true}}),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.