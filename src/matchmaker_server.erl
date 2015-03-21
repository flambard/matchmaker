-module(matchmaker_server).
-behaviour(gen_server).

%% API
-export([ start_link/2
        , start_link/3
        , find_match/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state,
        { pool
        , game_supervisor_module
        , game_settings_module
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(GameSupMod, GameSettingsMod) ->
    gen_server:start_link(?MODULE, [GameSupMod, GameSettingsMod], []).

start_link(Name, GameSupMod, GameSettingsMod) ->
    gen_server:start_link({local, Name},
                          ?MODULE,
                          [GameSupMod, GameSettingsMod],
                          []).

find_match(Server, Pid) ->
    gen_server:call(Server, {find_match, Pid}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([GameSupMod, GameSettingsMod]) ->
    {ok, #state{ pool = matchmaker_pool:new()
               , game_supervisor_module = GameSupMod
               , game_settings_module = GameSettingsMod
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({find_match, Pid}, _From, S = #state{pool = Pool}) ->
    MRef = monitor(process, Pid),
    NewPool =
        case matchmaker_pool:match_player(Pool, {Pid, MRef}) of
            {no_match, P} -> P;
            {match, Opponent, P} ->
                gen_server:cast(self(), {match_found, {{Pid, MRef}, Opponent}}),
                P
        end,
    {reply, ok, S#state{pool = NewPool}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({match_found, {{Player1, M1}, {Player2, M2}}}, S) ->
    demonitor(M1),
    demonitor(M2),
    %% TODO: Desired game settings should be included when registering in the
    %% matchmaker pool
    %% TODO: Handicap should be calculated by the difference in rank between
    %% the players
    SettingsMod = S#state.game_settings_module,
    SupMod = S#state.game_supervisor_module,
    SupMod:start_game(Player1, Player2, SettingsMod:new(), matchmaker),
    {noreply, S};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Monitor, _Type, Pid, _Info}, S = #state{pool = Pool}) ->
    NewPool = matchmaker_pool:remove_player(Pool, Pid),
    {noreply, S#state{pool = NewPool}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
