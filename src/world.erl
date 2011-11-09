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
    query_ants/1,
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
    ants,
    active_turn,
    no_players,
    score
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, [])
.

stop() -> gen_server:call(?MODULE, stop).

set_variable(Key, Val) ->
    gen_server:cast(?MODULE, {set_variable, Key, Val})
.

update_map(Elem, Args) ->
    gen_server:cast(?MODULE, {update_map, Elem, Args})
.

go() ->
%    gen_server:call(?MODULE, go)
    nop
.

query_ants() ->
    gen_server:call(?MODULE, query_ants)
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
        ants = []
    },
    {ok, State}
.

handle_info(_, S) ->
    {noreply, S}
.

handle_cast({update_map, food, [R, C]}, S) ->
    Map = S#state.dynamic_map,
    Settings = S#state.settings,
    NoCols = Settings#game_settings.cols,
    ets:insert(Map, {R*NoCols + C, food, S#state.active_turn}),
    {noreply, S}
;
handle_cast({update_map, water, [R, C]}, S) ->
    Map = S#state.static_map,
    Settings = S#state.settings,
    NoCols = Settings#game_settings.cols,
    ets:insert(Map, {R*NoCols + C, water, S#state.active_turn}),
    {noreply, S}
;
handle_cast({update_map, ant, [R, C, 0]}, S) ->
    AntsList = S#state.ants,
    NoCols = (S#state.settings)#game_settings.cols,
    Pos = R*NoCols + C,
    NAntsList = case proplists:is_defined(Pos, S#state.ants) of
        false ->
            {ok, AntPid} = ant:start_link(S#state.static_map),
            [{Pos, AntPid, R, C} | AntsList]
        ;
        true ->
            AntsList
    end,
    NState = S#state{ ants = NAntsList },
    DMap = S#state.dynamic_map,
    ets:insert(DMap, {Pos, ant, 0, S#state.active_turn}),
    {noreply, NState}
;
handle_cast({update_map, ant, [R, C, O]}, S) ->
    Map = S#state.dynamic_map,
    Settings = S#state.settings,
    NoCols = Settings#game_settings.cols,
    ets:insert(Map, {R*NoCols + C, ant, O, S#state.active_turn}),
    {noreply, S}
;
handle_cast({update_map, dead_ant, [R, C, O]}, S) ->
    Map = S#state.dynamic_map,
    NoCols = (S#state.settings)#game_settings.cols,
%    ets:insert(Map, {R*NoCols + C, ant, O, S#state.active_turn}),

    % TODO: if our ant dies kill ant process

    {noreply, S}
;
handle_cast({update_map, hill, [R, C, O]}, S) ->
    Map = S#state.dynamic_map,
    Settings = S#state.settings,
    NoCols = Settings#game_settings.cols,
    ets:insert(Map, {R*NoCols + C, hill, O, S#state.active_turn}),
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
    NewSettings = (S#state.settings)#game_settings{turntime = Val},
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
    {ok, NewS} = query_ants((S#state.settings)#game_settings.turntime),
    {reply, ok, NewS}
;
handle_call(query_ants, _, S) ->
    Fun = fun(AntPid, Row, Col) ->
        ant:get_decision(AntPid, {Row, Col}, 1)
    end,
    [
        Fun(APid, R, C)
    ||
        {Pos, APid, R, C} <- S#state.ants
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
