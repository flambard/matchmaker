-module(matchmaker).

%% API
-export([ new/0
        , find_match/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    matchmaker_sup:start_matchmaker_server().

find_match(Matchmaker) ->
    matchmaker_server:find_match(Matchmaker).


%%%===================================================================
%%% Internal functions
%%%===================================================================
