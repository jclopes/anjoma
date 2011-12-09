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
    turntime_micro, % same as turntime but in microseconds
    rows,
    cols,
    turns,
    viewradius2,
    attackradius2,
    spawnradius2,
    player_seed
}).

-record(state, {
    settings,      % game settings
    hive_map,
    my_hive_map,
    ants_map,
    my_ants_map,
    food_map,
    water_map,
    hives_pool,     % TODO: store hive pids
    ants_pool,      % ants that played last turn
    ants_alive,     % ants that are available for this turn
    active_turn,    % current turn number
    movements,      % positions our ants moved to this turn
    t0,             % time when turn started
    timer,          % timer that will send a message to triger the turn end
    no_players,
    score
}).

%%% %%% %%%
%% API
%%% %%% %%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, [])
.

stop() -> gen_server:call(?MODULE, stop).

set_variable(Key, Val) ->
    error_logger:info_msg("~s ~p", [Key, Val]),
    gen_server:cast(?MODULE, {set_variable, Key, Val})
.

update_map(Elem, Args) ->
    gen_server:cast(?MODULE, {update_map, Elem, Args})
.

go() ->
    gen_server:call(?MODULE, go, infinity)
.

dump_log() ->
    gen_server:call(?MODULE, dump_log)
.

%%% %%% %%%
%% Internal functions
%%% %%% %%%

finish_turn(S) ->
    % send the end turn message
    % kill all ants still in the pool (not alive)
    % set the ants_pool to ants_alive and reset ants_alive
    io:format("go~n"),
    AntsPool = S#state.ants_pool,
    AntsAlive = S#state.ants_alive,
    ets:delete_all_objects(S#state.food_map),
    lists:map(
        fun({_RC, AntPid}) ->
            ant:stop(AntPid)
        end,
        AntsPool
    ),
    S#state{
        ants_pool = AntsAlive,
        ants_alive = [],
        movements = [],
        active_turn = infinity
    }
.

move_ant({move, From, {Pos, RC}, {Pos, RC}, "", Turn}, S) ->
    % stay in the same place
    AntsAlive = S#state.ants_alive,
    Movements = S#state.movements,
    % check if movement generates colision
%    error_logger:info_msg("World: ~p ~p", [RC, Movements]),
    {NAntsAlive, NMovements} = case lists:member(RC, Movements) of
        true ->
            ant:solve_colision(From, self(), {Pos, RC}, Movements, Turn),
            {AntsAlive, Movements}
        ;
        false ->
%            error_logger:info_msg("World: stay ~p", [RC]),
            {
                AntsAlive,
                [RC | Movements]
            }
    end,
    S#state{ants_alive = NAntsAlive, movements = NMovements}
;
move_ant({move, From, {Pos, {R,C}=RC}, {_NPos, NRC}, D, Turn}, S) ->
    AntsAlive = S#state.ants_alive,
    Movements = S#state.movements,
    % check if movement generates colision
%    error_logger:info_msg("World: move from ~p ~s", [RC, D]),
%    error_logger:info_msg("World: ~p ~p", [NRC, Movements]),
    {NAntsAlive, NMovements} = case lists:member(NRC, Movements) of
        true ->
%            error_logger:info_msg("World: collision ~p ~p", [NRC, Movements]),
            ant:solve_colision(From, self(), {Pos, RC}, Movements, Turn),
            {AntsAlive, Movements}
        ;
        false ->
            io:format("o ~p ~p ~s~n", [R, C, D]),
            {
                [{NRC, From} | proplists:delete(RC, AntsAlive)],
                [NRC | Movements]
            }
    end,
    S#state{ants_alive = NAntsAlive, movements = NMovements}
.


%%% %%% %%%
%% gen_server API
%%% %%% %%%

init(no_args) ->
    HiveMap = ets:new(hive_map, []),
    AntsMap = ets:new(ants_map, []),
    MyHiveMap = ets:new(my_hive_map, []),
    MyAntsMap = ets:new(my_ants_map, []),
    FoodMap = ets:new(food_map, []),
    WaterMap = ets:new(water_map, []),
    State = #state{
        settings = #game_settings{},
        hive_map = HiveMap,
        ants_map = AntsMap,
        my_hive_map = MyHiveMap,
        my_ants_map = MyAntsMap,
        food_map = FoodMap,
        water_map = WaterMap,
        ants_pool = [],
        ants_alive = [],
        hives_pool = [],
        movements = []
    },
    {ok, State}
.

handle_info(timeout, S) ->
    NState = finish_turn(S),
    error_logger:info_msg("World: end of turn after: ~p", [timer:now_diff(now(), S#state.t0)]),
    timer:cancel(S#state.timer),
    {noreply, NState}
;
% move, From, Origin, Destiny, Direction, Turn
handle_info({move, _From, _Pos, _NPos, _D, Turn} = Msg, #state{active_turn=Turn}=S) ->
    % Do we have time?
    NewState = case 
        timer:now_diff(now(), S#state.t0) >= (S#state.settings)#game_settings.turntime_micro of
        true ->
            finish_turn(S)
        ;
        false ->
            move_ant(Msg, S)
    end,
    {noreply, NewState}
;
handle_info({move, _From, _Pos, _NPos, _D, Turn}, S) ->
    % Outdated move
%    error_logger:info_msg("World: move out of turn: AT=~p , MT=~p", [S#state.active_turn, Turn]),
    {noreply, S}
;
handle_info(Msg, S) ->
    error_logger:debug_info("World: not implemented ~p", [Msg]),
    {noreply, S}
.

handle_cast({update_map, food, [R, C]}, S) ->
    Map = S#state.food_map,
    MaxCol = (S#state.settings)#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C}),
    {noreply, S}
;
handle_cast({update_map, water, [R, C]}, S) ->
    Map = S#state.water_map,
    MaxCol = (S#state.settings)#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C}),
    {noreply, S}
;
handle_cast({update_map, ant, [R, C, 0]}, S) ->
    % if the ant is in our pool move it to the alive group
    % else spawn a new ant
    AntsPool = S#state.ants_pool,
    AntsAlive = S#state.ants_alive,
    MaxRow = (S#state.settings)#game_settings.rows,
    MaxCol = (S#state.settings)#game_settings.cols,
    MapSize = {MaxRow, MaxCol},
    Pos = pf:coord_to_pos(MapSize, {R, C}),
    {NAntsPool, NAntsAlive} = case proplists:is_defined({R,C}, AntsPool) of
        false ->
            {ok, AntPid} = ant:start_link(
                S#state.water_map,
                S#state.food_map,
                MapSize
            ),
            {AntsPool, [{{R,C}, AntPid} | AntsAlive]}
        ;
        true ->
            {proplists:delete({R,C}, AntsPool), [proplists:lookup({R,C}, AntsPool) | AntsAlive]}
    end,
    NState = S#state{ants_pool = NAntsPool, ants_alive = NAntsAlive},
    EMap = S#state.my_ants_map,
    ets:insert(EMap, {Pos}),
    {noreply, NState}
;
handle_cast({update_map, ant, [R, C, O]}, S) ->
    Map = S#state.ants_map,
    Settings = S#state.settings,
    MaxCol = Settings#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C}),
    {noreply, S}
;
%handle_cast({update_map, dead_ant, [R, C, 0]}, S) ->
%    Map = S#state.food_map,
%    MaxCol = (S#state.settings)#game_settings.cols,
%    ets:insert(Map, {R*MaxCol + C, ant, O, S#state.active_turn}),

    % TODO: if our ant dies kill ant process

%    {noreply, S}
%;
handle_cast({update_map, hill, [R, C, 0]}, S) ->
    Map = S#state.my_hive_map,
    Settings = S#state.settings,
    MaxCol = Settings#game_settings.cols,
    ets:insert(Map, {R*MaxCol + C}),
    {noreply, S}
;
handle_cast({update_map, hill, [R, C, O]}, S) ->
    WMap = S#state.water_map,
    AMap = S#state.ants_map,
    Map = S#state.hive_map,
    Settings = S#state.settings,
    MaxCol = Settings#game_settings.cols,
    MaxRow = Settings#game_settings.rows,
    HivesPool = S#state.hives_pool,
    NHivesPool = case
        ets:member(Map, {R, C})
    of
        true -> HivesPool;
        false ->
            error_logger:info_msg("found new hive ~p", [{R,C}]),
            ets:insert(Map, {{R,C}}),
            % start hill process
            {ok, HivePid} = hive:start_link(WMap, AMap, {MaxRow, MaxCol}, {R,C}),
            [HivePid | HivesPool]
    end,
    {noreply, S#state{hives_pool = NHivesPool}}
;
handle_cast({set_variable, "turn", 0}, S) ->
    {noreply, S#state{active_turn=0}}
;
handle_cast({set_variable, "turn", Val}, S) ->
    T0 = now(),
    TimeLimit = (S#state.settings)#game_settings.turntime,
    {ok, Tref} = timer:send_after(TimeLimit, timeout),
    {noreply, S#state{active_turn=Val, t0=T0, timer=Tref}}
;
handle_cast({set_variable, "players", Val}, S) ->
    {noreply,  S#state{no_players = Val}}
;
handle_cast({set_variable, "score", Val}, S) ->
    {noreply, S#state{score = Val}}
;
handle_cast({set_variable, "turntime", Val}, S) ->
    % take out X ms to be sure that we don't timeout and convert microseconds
    NewSettings = (S#state.settings)#game_settings{
        turntime = Val - 100,
        turntime_micro = (Val - 100)*1000
    },
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "loadtime", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{loadtime = Val},
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "rows", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{rows = Val},
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "cols", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{cols = Val},
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "turns", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{turns = Val},
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "viewradius2", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{viewradius2 = Val},
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "attackradius2", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{attackradius2 = Val},
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "spawnradius2", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{spawnradius2 = Val},
    {noreply, S#state{settings = NewSettings}}
;
handle_cast({set_variable, "player_seed", Val}, S) ->
    NewSettings = (S#state.settings)#game_settings{player_seed = Val},
    {noreply, S#state{settings = NewSettings}}
.

handle_call(go, _, #state{active_turn=infinity}=S) ->
    error_logger:info_msg("active_turn=infinity"),
    {reply, ok, S}
;
handle_call(go, _, S) ->
    lists:map(
        fun({RC, AntPid}) ->
            ant:get_decision(AntPid, self(), RC, S#state.active_turn)
        end,
        S#state.ants_alive
    ),
    lists:map(
        fun(HivePid) ->
            hive:get_decision(HivePid, self(), S#state.ants_alive, S#state.active_turn)
        end,
        S#state.hives_pool
    ),
    {reply, ok, S}
;
handle_call(dump_log, _, S) ->
    Dump = ets:tab2list(S#state.water_map),
    error_logger:info_msg("Water Map:~n~p~n", [Dump]),
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
