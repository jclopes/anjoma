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
    start_link/4,
    stop/1,
    get_decision/4,
    solve_colision/5
]).

-record(state, {
    max_col,
    max_row,
    dynamic_map,
    static_map
}).

%%% %%% %%%
%% API
%%% %%% %%%

start_link(StaticMap, DynamicMap, MaxRow, MaxCol) ->
    gen_server:start_link(?MODULE, [StaticMap, DynamicMap, MaxRow, MaxCol], [])
.

stop(Pid) -> gen_server:cast(Pid, stop).

get_decision(Pid, From, {R, C}, Turn) ->
    gen_server:cast(Pid, {get_decision, From, {R, C}, Turn})
.

solve_colision(Pid, From, {Pos, R, C}, Movements, Turn) ->
    gen_server:cast(Pid, {solve_colision, From, {Pos, R, C}, Movements, Turn})
.

%%% %%% %%%
%% Internal functions
%%% %%% %%%


food_direction(MapStatic, MapDynamic, {MaxRow, MaxCol}=MapSize, StartPos) ->
    % TODO: search depth should be smaller or equal to the viewing range
    {PathDict, TargetCell} = expand_cell(MapStatic, MapDynamic, MapSize, StartPos, 7),

    % TODO: store the path in state to avoid recalculate
    case trace_path(PathDict, TargetCell) of
        [A, B | Tail] ->
            error_logger:info_msg("Ant: path to: ~p -> ~p : ~p", [A, B, Tail]),
            {RC, Dir} = get_move(A, B),
            {coord_to_pos(RC, MaxCol), RC, Dir}
        ;
        [A] ->
            error_logger:info_msg("Ant: path to: ~p", [A]),
            {RC, Dir} = get_move(StartPos, A),
            {coord_to_pos(RC, MaxCol), RC, Dir}
    end
.

random_direction(Map, MaxCol, R, C) ->
    ValidDirs = lists:filter(
        fun({Pos, _RC, _D}) ->
            not ets:member(Map, Pos)
        end,
        get_NSEW(MaxCol, {R, C})
    ),
    Nth = random:uniform(length(ValidDirs)),
    lists:nth(Nth, ValidDirs)
.

get_move({R0, C0}, {R0, C1}) ->
    Dir = case C0 - C1 of
        1 -> "W";
        -1 -> "E";
        X when X < -1 -> "W";
        X when X > 1 -> "E"
    end,
    {{R0, C1}, Dir}
;
get_move({R0, C0}, {R1, C0}) ->
    Dir = case R0 - R1 of
        1 -> "N";
        -1 -> "S";
        X when X < -1 -> "N";
        X when X > 1 -> "S"
    end,
    {{R1, C0}, Dir}
.

coord_to_pos({R, C}, MaxCol) ->
    R*MaxCol + C
.

opposite_direction("N") -> "S";
opposite_direction("S") -> "N";
opposite_direction("E") -> "W";
opposite_direction("W") -> "E".

% deprecated way of calculating adjacencies
get_NSEW(MaxCol, {R, C}) ->
    N = {(R - 1)*MaxCol+C, {R - 1, C}, "N"},
    S = {(R + 1)*MaxCol+C, {R + 1, C}, "S"},
    E = {R*MaxCol + C + 1, {R, C + 1}, "E"},
    W = {R*MaxCol + C - 1, {R, C - 1}, "W"},
    [N,S,E,W]
.

% Return adjacent cells
% if it would go over one side of the map then reenter in the oposite side
get_adjacent_pos({MaxRow, MaxCol}, {R, C}) ->
    [
        case R - 1 < 1 of
            true -> {MaxRow, C};
            false -> {R - 1, C}
        end,
        case R + 1 > MaxRow of
            true -> {1, C};
            false -> {R + 1, C}
        end,
        case C + 1 > MaxCol of
            true -> {R, 1};
            false -> {R, C + 1}
        end,
        case C - 1 < 1 of
            true -> {R, MaxCol};
            false -> {R, C - 1}
        end
    ]
.

% This funtions implement a breadth-first search
expand_cell(MapStatic, MapDynamic, MapSize, StartPos, MaxDepth) ->
    expand_cell_acc(MapStatic, MapDynamic, MapSize, [StartPos], dict:store(StartPos, start_pos, dict:new()), MaxDepth)
.

expand_cell_acc(_, _, _, StartPoss, PathDict, 0) ->
    % TODO: get a better stratagy
    % if we reach the maximum depth, return last cell added as target.
    Nth = random:uniform(length(StartPoss) - 1),
    H = lists:nth(Nth, StartPoss),
    error_logger:info_msg("Target:~p PathDict:~p", [H, PathDict]),
    {PathDict, H}
;
expand_cell_acc(MapStatic, MapDynamic, {MaxRow, MaxCol}=MapSize, StartPoss, PathDict, MaxDepth) ->
    % TODO: Optimization, expand_cell_list_acc will give back already visited cells
    DictExpCells = expand_cell_list_acc(MapSize, StartPoss, PathDict),
    ExpCells = dict:fetch_keys(DictExpCells),
    % remove Cells that are water
    LandCellsDict = lists:foldl(
        fun(RC, Acc) ->
            case ets:member(MapStatic, coord_to_pos(RC, MaxCol)) of
                true -> dict:erase(RC, Acc);
                false -> Acc
            end
        end,
        DictExpCells,
        ExpCells
    ),
    LandCellsList = dict:fetch_keys(LandCellsDict),
    % check if we found food
    FoodCells = lists:filter(
        fun(RC) ->
            case ets:lookup(MapStatic, coord_to_pos(RC, MaxCol)) of
                {Pos, food, _Turn} ->
                    error_logger:info_msg("found food!"),
                    true
                ;
                _ -> false
            end
        end,
        LandCellsList
    ),
    case FoodCells of
        [] ->
        %    expand next level
            expand_cell_acc(MapStatic, MapDynamic, MapSize, LandCellsList, LandCellsDict, MaxDepth - 1)
        ;
        [FoodCell|_] ->
        %    get the path from the origin
            {LandCellsDict, FoodCell}
    end
.

% Given a MapSize and a list of starting positions,
% get all the adjacent cells to those starting positions.
% Returns a dict that maps Cell to OriginCell.
% This will be used to traceback the path to the origin.
expand_cell_list_acc(_MapSize, [], CellSet) ->
    CellSet
;
expand_cell_list_acc(MapSize, [OrigPos|Tail], Acc) ->
    NAcc = lists:foldl(
        fun(RC, CellSet) ->
            case dict:is_key(RC, CellSet) of
                false ->
                    dict:store(RC, OrigPos, CellSet)
                ;
                true ->
                    CellSet
            end
        end,
        Acc,
        get_adjacent_pos(MapSize, OrigPos)
    ),
    expand_cell_list_acc(MapSize, Tail, NAcc)
.

% Given a Path Dict and a target position
% returns a list with a traceback to the start position
trace_path(PathDict, TargetCell) ->
    trace_path_acc(PathDict, TargetCell, [TargetCell])
.

trace_path_acc(PathDict, TargetCell, Path) ->
    case dict:fetch(TargetCell, PathDict) of
        start_pos ->
            Path
        ;
        Prev ->
            trace_path_acc(PathDict, Prev, [Prev | Path])
    end
.

%%% %%% %%%
%% gen_server API
%%% %%% %%%

init([StaticMap, DynamicMap, MaxRow, MaxCol]) ->
    {R1,R2,R3} = now(),
    random:seed(R1,R2,R3),
    State = #state{
        dynamic_map = DynamicMap,
        static_map = StaticMap,
        max_row = MaxRow,
        max_col = MaxCol
    },
    {ok, State}
.

handle_info(_, S) ->
    {noreply, S}
.

handle_cast({get_decision, From, {R, C}, Turn}, S) ->
    SMap = S#state.static_map,
    DMap = S#state.dynamic_map,
    MaxRow = S#state.max_row,
    MaxCol = S#state.max_col,
    Pos = R*MaxCol + C,
    {NPos, {DR, DC}, D} = food_direction(SMap, DMap, {MaxRow, MaxCol}, {R, C}),
    From ! {move, self(), {Pos, R, C}, {NPos, DR, DC}, D, Turn},
    {noreply, S}
;
handle_cast({solve_colision, From, {Pos, R, C}, Movements, Turn}, S) ->
    % Colision: stay in the same place or find a free one
    MaxCol = S#state.max_col,
    PossibleDirs = lists:dropwhile(
        fun({X,_,_}) ->
            lists:member(X, Movements) or ets:member(S#state.static_map, X)
        end,
        get_NSEW(MaxCol, {R, C})
    ),
    case PossibleDirs of
        [] ->
            nop
        ;
        [{NPos, {DR, DC}, D} | _] ->
            From ! {move, self(), {Pos, R, C}, {NPos, DR, DC}, D, Turn}
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
