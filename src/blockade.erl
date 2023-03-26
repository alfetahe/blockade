-module(blockade).

-export([subscribe/3, dispatch/4]).

subscribe(StatemPid, EventKey, Opts) ->
    gen_statem:call(StatemPid, {add_event_subscriber, EventKey, self(), Opts}).

dispatch(StatemPid, EventKey, EventData, Opts) ->
    gen_statem:call(StatemPid, {dispatch_event, EventKey, EventData, Opts}).