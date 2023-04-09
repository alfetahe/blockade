-module(blockade_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(Args) ->
    {ok,
     {{one_for_one, 5, 10},
      [#{id => blockade_overviewer,
         start => {blockade_overviewer, start_link, [Args]}}]}}.
