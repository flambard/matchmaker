-module(matchmaker).

%% API
-export([ start_matchmaker/2
        , start_matchmaker/3
        , find_match/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

start_matchmaker(GameSupMod, GameSettingsMod) ->
    matchmaker_sup:start_matchmaker_server(GameSupMod, GameSettingsMod).

start_matchmaker(Name, GameSupMod, GameSettingsMod) ->
    matchmaker_sup:start_matchmaker_server(Name, GameSupMod, GameSettingsMod).

find_match(Matchmaker, Pid) ->
    matchmaker_server:find_match(Matchmaker, Pid).


%%%===================================================================
%%% Internal functions
%%%===================================================================
