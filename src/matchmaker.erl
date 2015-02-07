-module(matchmaker).

%% API
-export([ new/0
        , find_match/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    matchmaker_sup:start_matchmaker_server().

find_match(Matchmaker, Pid) ->
    matchmaker_server:find_match(Matchmaker, Pid).


%%%===================================================================
%%% Internal functions
%%%===================================================================
