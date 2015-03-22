-module(matchmaker).

%% API
-export([ start_matchmaker/1
        , start_matchmaker/2
        , find_match/3
        ]).


-callback start_game({Player1 :: pid(), Info1 :: term()},
                     {Player2 :: pid(), Info2 :: term()}) ->
    ok.

-type matchmaker_ref() :: atom() | pid().


%%%===================================================================
%%% API
%%%===================================================================

-spec start_matchmaker(CallbackMod :: module()) ->
                              {ok, Matchmaker :: pid()} |
                              {error, term()}.
start_matchmaker(CallbackMod) ->
    matchmaker_sup:start_matchmaker_server(CallbackMod).

-spec start_matchmaker(Name :: atom(), CallbackMod :: module()) ->
                              {ok, Matchmaker :: pid()} |
                              {error, term()}.
start_matchmaker(Name, CallbackMod) ->
    matchmaker_sup:start_matchmaker_server(Name, CallbackMod).

-spec find_match(MatchmakerRef :: matchmaker_ref(),
                 Player :: pid(),
                 Info :: term()) ->
                        ok.
find_match(MatchmakerRef, Player, Info) ->
    matchmaker_server:find_match(MatchmakerRef, Player, Info).


%%%===================================================================
%%% Internal functions
%%%===================================================================
