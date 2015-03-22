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

-spec start_matchmaker(Module :: module()) ->
                              {ok, Matchmaker :: pid()} |
                              {error, term()}.
start_matchmaker(Module) ->
    matchmaker_sup:start_matchmaker_server(Module).

-spec start_matchmaker(Name :: atom(), Module :: module()) ->
                              {ok, Matchmaker :: pid()} |
                              {error, term()}.
start_matchmaker(Name, Module) ->
    matchmaker_sup:start_matchmaker_server(Name, Module).

-spec find_match(MatchmakerRef :: matchmaker_ref(),
                 Player :: pid(),
                 Info :: term()) ->
                        ok.
find_match(MatchmakerRef, Player, Info) ->
    matchmaker_server:find_match(MatchmakerRef, Player, Info).


%%%===================================================================
%%% Internal functions
%%%===================================================================
