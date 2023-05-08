-module(blockade_service).

-include("include/blockade_header.hrl").

-export([rand_node/0, get_sync_priority/2]).

rand_node() ->
    Nodes = erlang:nodes(),
    lists:nth(
        rand:uniform(length(Nodes)), Nodes).

get_sync_priority(Lp, Man) ->
    case remote_priority(Man) of
        Lp ->
            Lp;
        undefined ->
            Lp;
        Rp ->
            % If there are only 2 nodes in the cluster then agree with the
            % remote priority. Otherwise, check the remote priority again.
            case length(erlang:nodes()) of
                1 ->
                    Rp;
                _ ->
                    Rp2 = remote_priority(Man),
                    [Sp | _] = most([Lp, Rp, Rp2]),
                    Sp
            end
    end.

remote_priority(Manager) ->
    Nodes = erlang:nodes(),
    if length(Nodes) == 0 ->
            ?DEFAULT_PRIORITY;
        true ->
            RandNode =
                lists:nth(
                    rand:uniform(length(Nodes)), Nodes),
            Rm = gen_server:call({RandNode, Manager}, get_priority, ?GEN_CALL_TIMEOUT),
            case Rm of
                undefined ->
                    ?DEFAULT_PRIORITY;
                _ ->
                    gen_server:call({RandNode, Manager}, get_priority, ?GEN_CALL_TIMEOUT)
            end
    end.

most(List) ->
    Lc = lists:foldl(fun(E1, E2) -> maps:put(E1, maps:get(E1, E2, 0) + 1, E2) end, #{}, List),
    Ls = lists:sort(fun({_, Ac}, {_, Bc}) -> Ac > Bc end, maps:to_list(Lc)),
    [Value || {Value, _Count} <- Ls].
        