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
