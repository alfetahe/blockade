%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------
-define(DEFAULT_PRIORITY, 0).
-define(DEFAULT_DISCARD_EVENTS, false).
-define(EVENT_QUEUE_PRUNE, 3000).
-define(PRIORITY_SYNC, 10000).
-define(GEN_CALL_TIMEOUT, 3000).
-define(PROCESS_NAME(Name, Suffix),
        erlang:list_to_atom(erlang:atom_to_list(Name) ++ "_" ++ Suffix)).

%%------------------------------------------------------------------------------
%% Record definitions
%%------------------------------------------------------------------------------
-record(manrec,
        {manager :: blockade:event_manager(),
         event_queue = [] :: list(),
         priority = ?DEFAULT_PRIORITY :: integer(),
         discard_events = ?DEFAULT_DISCARD_EVENTS,
         schduler_ref = undefined :: reference() | undefined}).
