-module(matchmaker).

%% API
-export([ start_matchmaker/1
        , start_matchmaker/2
        , find_match/3
        ]).


-callback start_game({Player1 :: pid(), Info1 :: term()},
                     {Player2 :: pid(), Info2 :: term()}) ->
    ok.


%%%===================================================================
%%% API
%%%===================================================================

start_matchmaker(CallbackMod) ->
    matchmaker_sup:start_matchmaker_server(CallbackMod).

start_matchmaker(Name, CallbackMod) ->
    matchmaker_sup:start_matchmaker_server(Name, CallbackMod).

find_match(Matchmaker, Pid, Info) ->
    matchmaker_server:find_match(Matchmaker, Pid, Info).


%%%===================================================================
%%% Internal functions
%%%===================================================================
