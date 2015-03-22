-module(matchmaker).

%% API
-export([ start_matchmaker/1
        , start_matchmaker/2
        , find_match/2
        ]).


-callback start_game(Player1 :: pid(), Player2 ::pid()) ->
    ok.


%%%===================================================================
%%% API
%%%===================================================================

start_matchmaker(CallbackMod) ->
    matchmaker_sup:start_matchmaker_server(CallbackMod).

start_matchmaker(Name, CallbackMod) ->
    matchmaker_sup:start_matchmaker_server(Name, CallbackMod).

find_match(Matchmaker, Pid) ->
    matchmaker_server:find_match(Matchmaker, Pid).


%%%===================================================================
%%% Internal functions
%%%===================================================================
