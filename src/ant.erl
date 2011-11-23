-module(ant).

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
    start_link/3,
    stop/1,
    get_decision/4,
    solve_colision/5
]).

-record(state, {
    max_col,
    max_row,
    food_map,
    water_map,
    current_path
}).

%%% %%% %%%
%% API
%%% %%% %%%

start_link(WaterMap, FoodMap, MapSize) ->
    gen_server:start_link(?MODULE, [WaterMap, FoodMap, MapSize], [])
.

stop(Pid) -> gen_server:cast(Pid, stop).

get_decision(Pid, From, {R, C}, Turn) ->
    gen_server:cast(Pid, {get_decision, From, {R, C}, Turn})
.

solve_colision(Pid, From, {Pos, RC}, Movements, Turn) ->
    gen_server:cast(Pid, {solve_colision, From, {Pos, RC}, Movements, Turn})
.

%%% %%% %%%
%% Internal functions
%%% %%% %%%

food_path(WaterMap, FoodMap, MapSize, StartPos) ->
    % TODO: search depth should be smaller or equal to the viewing range
    PathDict = pf:get_paths(WaterMap, MapSize, StartPos, 7),
    FoodCells = lists:filter(
        fun(RC) ->
            ets:member(FoodMap, pf:coord_to_pos(MapSize, RC))
        end,
        dict:fetch_keys(PathDict)
    ),
    case FoodCells of
        [] -> % no food go to most distant direction
            {DistantRC, _} = dict:fold(
                fun(K, {_, Depth}, {MK, MaxDepth}) ->
                    case Depth > MaxDepth of
                        true ->
                            {K, Depth}
                        ;
                        false ->
                            {MK, MaxDepth}
                    end
                end,
                {undefined, -1},
                PathDict
            ),
            pf:extract_path(PathDict, DistantRC)
        ;
        [F|_] ->
            pf:extract_path(PathDict, F)
    end
.

%%% %%% %%%
%% gen_server API
%%% %%% %%%

init([WaterMap, FoodMap, {MaxRow, MaxCol}]) ->
    {R1,R2,R3} = now(),
    random:seed(R1,R2,R3),
    State = #state{
        food_map = FoodMap,
        water_map = WaterMap,
        max_row = MaxRow,
        max_col = MaxCol,
        current_path = []
    },
    {ok, State}
.

handle_info(_, S) ->
    {noreply, S}
.

handle_cast({get_decision, From, RC, Turn}, S) ->
    WMap = S#state.water_map,
    FMap = S#state.food_map,
    MapSize = {S#state.max_row, S#state.max_col},
    CurPath = S#state.current_path,
    Pos = pf:coord_to_pos(MapSize, RC),

    {NPath, {NPos, NRC, D}} = case CurPath of
        [RC0, RC1|PathTail] ->
            Dir = pf:get_direction(RC0, RC1),
            {[RC1|PathTail], {pf:coord_to_pos(MapSize, RC1), RC1, Dir}}
        ;
        _ ->
            [RC0, RC1|PathTail] = 
            food_path(WMap, FMap, MapSize, RC),
            Dir = pf:get_direction(RC0, RC1),
            {[RC1|PathTail], {pf:coord_to_pos(MapSize, RC1), RC1, Dir}}
    end,
    From ! {move, self(), {Pos, RC}, {NPos, NRC}, D, Turn},
    NState = S#state{current_path = NPath},
    {noreply, NState}
;
handle_cast({solve_colision, From, {Pos, RC}, Movements, Turn}, S) ->
    % Colision: stay in the same place or find a free one
    MapSize = {S#state.max_row, S#state.max_col},
    PossibleDirs = lists:dropwhile(
        fun(XY) ->
            P = pf:coord_to_pos(MapSize, XY),
            lists:member(P, Movements) or ets:member(S#state.water_map, P)
        end,
        pf:get_adjacent(MapSize, RC)
    ),
    case PossibleDirs of
        [] ->
            nop
        ;
        [NRC | _] ->
            NPos = pf:coord_to_pos(MapSize, NRC),
            D = pf:get_direction(RC, NRC),
            From ! {move, self(), {Pos, RC}, {NPos, NRC}, D, Turn}
    end,
    {noreply, S}
;
handle_cast(stop, S) ->
    {stop, normal, S}
.

handle_call(Req, _, S) ->
    error_logger:debug_info("Not implemented: call ~p", [Req]),
    {reply, undefined, S}
.

terminate(_Reason, _S) ->
    ok
.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}
.
