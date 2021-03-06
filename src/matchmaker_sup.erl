-module(matchmaker_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0
        , start_matchmaker_server/1
        , start_matchmaker_server/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_matchmaker_server(CallbackMod) ->
    ChildSpec = {make_ref(),
                 {matchmaker_server, start_link, [CallbackMod]},
                 transient,
                 2000,
                 worker,
                 [matchmaker_server]},
    supervisor:start_child(?SERVER, ChildSpec).

start_matchmaker_server(Name, CallbackMod) ->
    ChildSpec = {make_ref(),
                 {matchmaker_server, start_link, [Name, CallbackMod]},
                 transient,
                 2000,
                 worker,
                 [matchmaker_server]},
    supervisor:start_child(?SERVER, ChildSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {one_for_one, 1000, 3600},
    {ok, {SupFlags, []}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
