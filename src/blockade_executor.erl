-module(blockade_executor).

-export([execute_callbacks/4, execute_callback/4]).

execute_callbacks([Pid | Pids], EventKey, EventData, SubsOpts) ->
    spawn_executor(Pid, EventKey, EventData, SubsOpts),
    execute_callbacks(Pids, EventKey, EventData, SubsOpts);
execute_callbacks([], _EventKey, _EventData, _SubsOpts) ->
    ok.

execute_callback(Pid,
                 EventKey,
                 EventData,
                 #{handler_type := gen_call = _SubsOpts}) ->
    gen_server:call(Pid, {EventKey, EventData});
execute_callback(Pid,
                 EventKey,
                 EventData,
                 #{handler_type := gen_cast = _SubsOpts}) ->
    gen_server:cast(Pid, {EventKey, EventData});
execute_callback(Pid, EventKey, EventData, _SubsOpts) ->
    Pid ! {EventKey, EventData}.

spawn_executor(Pid, EventKey, EventData, SubsOpts) ->
    SubOpts = maps:get(Pid, SubsOpts, #{}),
    erlang:spawn(?MODULE,
                 execute_callback,
                 [Pid, EventKey, EventData, SubOpts]).
