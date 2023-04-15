-module(blockade_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(#{name := Name} = Args) ->
    % TODO: patternmatch all mandatory keys.
    {ok,
     {{one_for_one, 5, 10},
      [#{id => blockade_event_manager,
         start => {blockade_event_manager, start_link, [Args]}},
       #{id => blockade_pg, start => {pg, start_link, [Name]}}]}};
init(_) ->
    throw(mandatory_keys_missing).
