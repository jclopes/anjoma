-module(world).

-behaviour(gen_server).

-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).

-export([
    start_link/0,
    stop/0,
    set_variable/2,
    update_map/2,
    go/0,
    dump_log/0
]).

-record(game_settings, {
    loadtime,
    turntime,
    rows,
    cols,
    turns,
    viewradius2,
    attackradius2,
    spawnradius2,
    player_seed
}).

-record(state, {
    settings,
    dynamic_map,
    static_map,
    ants_pool,
    ants_alive,
    active_turn,
    movements,
    no_players,
    score
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, [])
.

stop() -> gen_server:call(?MODULE, stop).

set_variable(Key, Val) ->
    error_logger:info_msg("~p = ~p~n", [Key, Val]),
    gen_server:cast(?MODULE, {set_variable, Key, Val})
.

update_map(Elem, Args) ->
    gen_server:cast(?MODULE, {update_map, Elem, Args})
.

go() ->
    gen_server:call(?MODULE, go)
.

dump_log() ->
    gen_server:call(?MODULE, dump_log)
.

%%% %%% %%%

init(no_args) ->
    DynamicMap = ets:new(dynamic_map, []),
    StaticMap = ets:new(static_map, []),
    error_logger:info_msg("~p~n~p~n", [DynamicMap, StaticMap]),
    State = #state{
        settings = #game_settings{},
        dynamic_map = DynamicMap,
        static_map = StaticMap,
        ants_pool = [],
        ants_alive = [],
        movements = []
    },
    {ok, State}
.

handle_info(finish_turn, S) ->
    % send the end turn message
    % kill all ants still in the pool (not alive)
    % set the ants_pool to ants_alive and reset ants_alive
    io:format("go~n"),
    AntsPool = S#state.ants_pool,
    AntsAlive = S#state.ants_alive,
    error_logger:info_msg("World: Pool=~p Alive=~p", [AntsPool, AntsAlive]),
    F = fun({_Pos, AntPid, _R, _C}) ->
        ant:stop(AntPid)
    end,
    [
        F(X)
    ||
        X <- AntsPool
    ],
    {noreply, S#state{
        ants_pool = AntsAlive,
        ants_alive = [],
        movements = [],
        active_turn = infinity
    }}
;
% move, From, Origin, Destiny, Direction, Turn
handle_info({move, From, {R, C}, {DR, DC}, D, Turn}, S) ->
    ActiveTurn = S#state.active_turn,
    AntsAlive = S#state.ants_alive,
    Movements = S#state.movements,
    MaxCol = (S#state.settings)#game_settings.cols,
    Pos = R*MaxCol + C,
    NPos = DR*MaxCol + DC,
    {NAntsAlive, NMovements} = case Turn of
        ActiveTurn ->
            error_logger:info_msg("World: move ~p ~p ~s", [R,C,D]),
            % check if movement generates colision
            case lists:member(NPos, Movements) of
                true ->
                    % Colision
                    {AntsAlive, Movements}
                ;
                false ->
                    io:format("o ~p ~p ~s~n", [R, C, D]),
                    {
                        [{NPos, From, DR, DC} | proplists:delete(Pos, AntsAlive)],
                        [NPos | Movements]
                    }
            end
        ;
        _ ->
            error_logger:info_msg("World: move out of turn: AT=~s , MT=~s", [ActiveTurn, Turn]),
            {AntsAlive, Movements}
    end,
    {noreply, S#state{ants_alive = NAntsAlive, movements = NMovements}}
;
handle_info(Msg, S) ->
    error_logger:debug_info("World: not implemented ~p~n", [Msg]),
    {noreply, S}
.

handle_cast({update_map, food, [R, C]}, S) ->
    Map = S#state.dynamic_map,
    MaxCol = (S#state.settings)#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C, food, S#state.active_turn}),
    {noreply, S}
;
handle_cast({update_map, water, [R, C]}, S) ->
    Map = S#state.static_map,
    MaxCol = (S#state.settings)#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C, water, S#state.active_turn}),
    {noreply, S}
;
handle_cast({update_map, ant, [R, C, 0]}, S) ->
    % if the ant is in our pool move it to the alive group
    % else spawn a new ant
    AntsPool = S#state.ants_pool,
    AntsAlive = S#state.ants_alive,
    MaxRow = (S#state.settings)#game_settings.rows,
    MaxCol = (S#state.settings)#game_settings.cols,
    Pos = R*MaxCol + C,
    {NAntsPool, NAntsAlive} = case proplists:is_defined(Pos, AntsPool) of
        false ->
            {ok, AntPid} = ant:start_link(
                S#state.static_map,
                S#state.dynamic_map,
                MaxRow,
                MaxCol
            ),
            {AntsPool, [{Pos, AntPid, R, C} | AntsAlive]}
        ;
        true ->
            {proplists:delete(Pos, AntsPool), [proplists:lookup(Pos, AntsPool) | AntsAlive]}
    end,
    NState = S#state{ants_pool = NAntsPool, ants_alive = NAntsAlive},
    DMap = S#state.dynamic_map,
    ets:insert(DMap, {Pos, ant, 0, S#state.active_turn}),
    {noreply, NState}
;
handle_cast({update_map, ant, [R, C, O]}, S) ->
    Map = S#state.dynamic_map,
    Settings = S#state.settings,
    MaxCol = Settings#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C, ant, O, S#state.active_turn}),
    {noreply, S}
;
handle_cast({update_map, dead_ant, [R, C, 0]}, S) ->
    Map = S#state.dynamic_map,
    MaxCol = (S#state.settings)#game_settings.cols,
%    ets:insert(Map, {R*MaxCol + C, ant, O, S#state.active_turn}),

    % TODO: if our ant dies kill ant process

    {noreply, S}
;
handle_cast({update_map, hill, [R, C, O]}, S) ->
    Map = S#state.dynamic_map,
    Settings = S#state.settings,
    MaxCol = Settings#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C, hill, O, S#state.active_turn}),
    {noreply, S}
;
handle_cast({set_variable, "turn", Val}, S) ->
    NewS = S#state{active_turn = Val},
    {noreply, NewS}
;
handle_cast({set_variable, "players", Val}, S) ->
    NewS = S#state{no_players = Val},
    {noreply, NewS}
;
handle_cast({set_variable, "score", Val}, S) ->
    NewS = S#state{score = Val},
    {noreply, NewS}
;
handle_cast({set_variable, "loadtime", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{loadtime = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "turntime", Val}, S) ->
    % take out X ms to be sure that we don't timeout
    NewSettings = (S#state.settings)#game_settings{turntime = Val - 100},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "rows", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{rows = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "cols", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{cols = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "turns", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{turns = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "viewradius2", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{viewradius2 = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "attackradius2", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{attackradius2 = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "spawnradius2", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{spawnradius2 = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
;
handle_cast({set_variable, "player_seed", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{player_seed = Val},
    NewS = S#state{settings = NewSettings},
    {noreply, NewS}
.

handle_call(go, _, S) ->
    Timeout = (S#state.settings)#game_settings.turntime,
    {ok, Tref} = timer:send_after(Timeout, finish_turn),
    SMap = S#state.static_map,
    DMap = S#state.dynamic_map,
    ATurn = S#state.active_turn,
    
    F = fun(AntPid, Row, Col) ->
        ant:get_decision(AntPid, self(), {Row, Col}, ATurn)
    end,
    [
        F(APid, R, C)
    ||
        {Pos, APid, R, C} <- S#state.ants_alive
    ],
    {reply, ok, S}
;
handle_call(dump_log, _, S) ->
    Dump = ets:tab2list(S#state.static_map),
    error_logger:info_msg("Static table:~n~p~n~n", [Dump]),
    {reply, ok, S}
;
handle_call(stop, _, S) ->
    {stop, normal, S}
.

terminate(_Reason, _S) ->
    ok
.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}
.
