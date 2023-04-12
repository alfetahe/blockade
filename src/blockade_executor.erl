-module(blockade_executor).

-export([execute_callbacks/3, execute_callback/3]).

execute_callbacks([Callback | Callbacks], EventKey, EventData) ->
    spawn_executor(Callback, EventKey, EventData),
    execute_callbacks(Callbacks, EventKey, EventData);
execute_callbacks([], _EventKey, _EventData) ->
    ok.

execute_callback({Pid, #{handler_type := gen_call} = _Opts},
                 EventKey,
                 EventData) ->
    gen_server:call(Pid, {EventKey, EventData});
execute_callback({Pid, #{handler_type := gen_cast} = _opts},
                 EventKey,
                 EventData) ->
    gen_server:cast(Pid, {EventKey, EventData});
execute_callback({Pid, _Opts}, EventKey, EventData) ->
    Pid ! {EventKey, EventData}.

spawn_executor(Callback, EventKey, EventData) ->
    erlang:spawn(?MODULE, execute_callback, [Callback, EventKey, EventData]).
