-module(matchmaker_server).
-behaviour(gen_server).

%% API
-export([ start_link/1
        , start_link/2
        , find_match/3
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
        { callback_module
        , pool
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(CallbackMod) ->
    gen_server:start_link(?MODULE, [CallbackMod], []).

start_link(Name, CallbackMod) ->
    gen_server:start_link({local, Name},
                          ?MODULE,
                          [CallbackMod],
                          []).

find_match(Server, Pid, Info) ->
    gen_server:call(Server, {find_match, Pid, Info}).


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
init([CallbackMod]) ->
    {ok, #state{ callback_module = CallbackMod
               , pool = matchmaker_pool:new()
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
handle_call({find_match, Pid, Info}, _From, S = #state{pool = Pool}) ->
    MRef = monitor(process, Pid),
    NewPool =
        case matchmaker_pool:match_player(Pool, {Pid, Info, MRef}) of
            {no_match, P} -> P;
            {match, Opponent, P} ->
                gen_server:cast(self(),
                                {match_found, {{Pid, Info, MRef}, Opponent}}),
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
handle_cast({match_found, {{Player1, Info1, M1}, {Player2, Info2, M2}}}, S) ->
    demonitor(M1),
    demonitor(M2),
    Mod = S#state.callback_module,
    Mod:start_game({Player1, Info1}, {Player2, Info2}),
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
