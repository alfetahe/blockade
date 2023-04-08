-module(blockade).

-export([subscribe/3, dispatch/3]).

subscribe(StatemPid, EventKey, Opts) ->
    gen_statem:call(StatemPid, {add_sub, EventKey, self(), Opts}).

dispatch(StatemPid, EventKey, EventData) ->
    gen_statem:call(StatemPid, {dispatch, EventKey, EventData}).
