-module(blockade_sup).

-behaviour(supervisor).

-export([start_link/1, start_manager/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(#{name := Name} = Args) ->
    {ok,
     {{one_for_one, 5, 10},
      [#{id => overviewer_name(Name),
         start => {blockade_overviewer, start_link, [Args]}},
        #{id => Name, start => {blockade_manager, start_link, [Args]}}]}}.

start_manager(#{name := Name} = Args) ->
    supervisor:start_child(blockade_sup,
                           #{id => Name,
                             start => {blockade_manager, start_link, [Args]}}).

overviewer_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_overviewer").
