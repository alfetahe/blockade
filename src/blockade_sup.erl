-module(blockade_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(#{name:=Name} = Args) ->
    {ok, {{one_for_one, 5, 10}, [
        #{id=>Name, start=>{blockade_manager, start_link, [Args]}}
        ]}}.