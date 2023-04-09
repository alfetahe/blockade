-module(blockade_executor).

-export([spawn/1, execute_callbacks/1]).

spawn(Callbacks) ->
    erlang:spawn(?MODULE, execute_callbacks, [Callbacks]).

execute_callbacks([Callback | Callbacks]) ->
    Callback(),
    execute_callbacks(Callbacks);
execute_callbacks([]) ->
    ok.
