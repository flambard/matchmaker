-module(matchmaker).

%% API
-export([ new/2
        , find_match/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

new(GameSupMod, GameSettingsMod) ->
    matchmaker_sup:start_matchmaker_server(GameSupMod, GameSettingsMod).

find_match(Matchmaker, Pid) ->
    matchmaker_server:find_match(Matchmaker, Pid).


%%%===================================================================
%%% Internal functions
%%%===================================================================
