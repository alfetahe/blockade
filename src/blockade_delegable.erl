-module(blockade_delegable).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, handle_continue/2]).

start_link(#{name := Name} = Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

init(_Args) ->
    {ok, #{}}.    