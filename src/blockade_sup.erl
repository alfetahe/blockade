-module(blockade_sup).

-include("blockade_header.hrl").

-define(SUFFIX, "sup").

-behaviour(supervisor).

-export([start_link/1, stop/1]).
-export([init/1]).

start_link(#{name := Name} = Args) ->
    supervisor:start_link({local, ?PROCESS_NAME(Name, ?SUFFIX)}, ?MODULE, Args).

init(#{name := Name} = Args) ->
    % TODO: patternmatch all mandatory keys.
    {ok,
     {{one_for_one, 5, 10},
      [#{id => blockade_event_manager, start => {blockade_event_manager, start_link, [Args]}},
       #{id => blockade_pg, start => {pg, start_link, [?PROCESS_NAME(Name, "pg")]}}]}};
init(_) ->
    throw(mandatory_keys_missing).

stop(Name) ->
    Pid = erlang:whereis(?PROCESS_NAME(Name, ?SUFFIX)),
    case Pid of
        undefined ->
            {error, not_found};
        SupPid ->
            exit(SupPid, shutdown)
    end.
