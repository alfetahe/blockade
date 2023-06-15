-module(blockade_sup).

-include("blockade_header.hrl").

-define(SUFFIX, "sup").

-behaviour(supervisor).

-export([start_link/1, start_link/2, stop/1]).
-export([init/1]).

start_link(Name) ->
    start_link(Name, #{}).

start_link(Name, Opts) ->
    supervisor:start_link({local, ?PROCESS_NAME(Name, ?SUFFIX)},
                          ?MODULE,
                          maps:put(name, Name, Opts)).

init(#{name := Name} = Args) ->
    {ok,
     {{one_for_one, 5, 10},
      [#{id => blockade_event_manager, start => {blockade_event_manager, start_link, [Args]}},
       #{id => blockade_pg, start => {pg, start_link, [?PROCESS_NAME(Name, "pg")]}}]}}.

stop(Name) ->
    Pid = erlang:whereis(?PROCESS_NAME(Name, ?SUFFIX)),
    case Pid of
        undefined ->
            {error, not_found};
        SupPid ->
            supervisor:terminate_child(SupPid, blockade_event_manager),
            supervisor:terminate_child(SupPid, blockade_pg),
            exit(SupPid, normal)
    end.
