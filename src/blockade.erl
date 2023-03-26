-module(blockade).

-export([subscribe/3]).

subscribe(StatemPid, EventKey, Opts) ->
    gen_statem:send_request(StatemPid, {add_event_subscriber, EventKey, self(), Opts}).