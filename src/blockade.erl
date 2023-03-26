-module(blockade).

-export([subscribe/3, emit/3]).

subscribe(StatemPid, EventKey, Opts) ->
    gen_statem:call(StatemPid, {add_event_subscriber, EventKey, self(), Opts}).

emit(StatemPid, EventKey, EventData) ->
    gen_statem:call(StatemPid, {event, EventKey, EventData}).