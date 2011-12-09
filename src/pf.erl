-module(pf).

-export([
    coord_to_pos/2,
    cell_dist/2,
    get_adjacent/2,
    get_direction/2,
    get_paths/4,
    get_path/4,
    extract_path/2
]).

%% Get the direction to go from Origin to Destination
%% if it reach the map limit then reenters in the oposite side.
get_direction(RC, RC) ->
    "" % direction to the same position is the empty string
;
get_direction({R0, C0}, {R0, C1}) ->
    case C0 - C1 of
        1 -> "W";
        -1 -> "E";
        X when X < -1 -> "W";
        X when X > 1 -> "E"
    end
;
get_direction({R0, C0}, {R1, C0}) ->
    case R0 - R1 of
        1 -> "N";
        -1 -> "S";
        X when X < -1 -> "N";
        X when X > 1 -> "S"
    end
.

%% Convert from coords to position
coord_to_pos({_MaxRow, MaxCol}, {R, C}) ->
    R*MaxCol + C
.

cell_dist({R0, C0}, {R1, C1}) ->
    abs(R1 - R0) + abs(C1 - C0)
.

%% Return all adjacent cells to the original
%% if it reach the map limit then reenters in the oposite side.
get_adjacent({MaxRow, MaxCol}, {R, C}) ->
    [
        case R - 1 < 0 of
            true -> {MaxRow - 1, C};
            false -> {R - 1, C}
        end,
        case R + 1 >= MaxRow of
            true -> {0, C};
            false -> {R + 1, C}
        end,
        case C + 1 >= MaxCol of
            true -> {R, 0};
            false -> {R, C + 1}
        end,
        case C - 1 < 0 of
            true -> {R, MaxCol - 1};
            false -> {R, C - 1}
        end
    ]
.

%% Returns a dict that maps all cells that are adjacent to SeedCells
%% and are not in AvoidCells
expand_cells(MapSize, SeedCells, AvoidCells) ->
    expand_cells_acc(MapSize, SeedCells, AvoidCells, dict:new())
.

expand_cells_acc(_, [], _, Acc) -> Acc;
expand_cells_acc(MapSize, [Seed|Seeds], AvoidCells, Acc) ->
    AdjacentCells = get_adjacent(MapSize, Seed),
    NAcc = lists:foldl(
        fun(Cell, A) ->
            case lists:member(Cell, AvoidCells) of
                true -> A;
                false -> dict:store(Cell, Seed, A)
            end
        end,
        Acc,
        AdjacentCells
    ),
    expand_cells_acc(MapSize, Seeds, AvoidCells, NAcc)
.


%% Find all paths from Origin with distance less then MaxDepth
%% Map -> ets with the obstacles
%% MapSize -> {MaxRow, MaxCol}
%% Origin -> {Row, Col}
%% MaxDepth -> int() > 0
%% returns -> dict()
get_paths(Map, MapSize, Origin, MaxDepth) ->
    ValidPaths = dict:store(Origin, {origin,0}, dict:new()),
    AvoidCells = [Origin],
    get_paths_acc(Map, MapSize, [Origin], 1, MaxDepth, ValidPaths, AvoidCells)
.

get_paths_acc(_M, _MS, _O, MaxDepth, MaxDepth, ValidPaths, _BC) ->
    ValidPaths
;
get_paths_acc(Map, MapSize, SeedCells, CurDepth, MaxDepth, ValidPaths, AvoidCells) ->
    NewPaths = expand_cells(MapSize, SeedCells, AvoidCells),
    {NValidPaths, NSeedCells, NAvoidCells} = dict:fold(
        fun(RC, V, {VP, NSC, AC})->
            case ets:member(Map, coord_to_pos(MapSize, RC)) of
                true ->
                    {VP, NSC, [RC|AC]};
                false ->
                    {dict:store(RC, {V, CurDepth}, VP), [RC|NSC], [RC|AC]}
            end
        end,
        {ValidPaths, [], AvoidCells},
        NewPaths
    ),
    get_paths_acc(Map, MapSize, NSeedCells, CurDepth + 1, MaxDepth, NValidPaths, NAvoidCells)
.

% Get a path from RC0 to RC1
% WARNING - no depth limit can be heavy or never finish
get_path(Map, MapSize, RC0, RC1) ->
    ValidPaths = dict:store(RC0, {origin,0}, dict:new()),
    AvoidCells = [RC0],
    PathDict = get_path_acc(Map, MapSize, [RC0], RC1, 1, ValidPaths, AvoidCells),
    extract_path(PathDict, RC1)
.
get_path_acc(Map, MapSize, SeedCells, Destination, CurDepth, ValidPaths, AvoidCells) ->
    NewPaths = expand_cells(MapSize, SeedCells, AvoidCells),
    {NValidPaths, NSeedCells, NAvoidCells} = dict:fold(
        fun(RC, V, {VP, NSC, AC})->
            case ets:member(Map, coord_to_pos(MapSize, RC)) of
                true -> {VP, NSC, [RC|AC]};
                false ->
                    {dict:store(RC, {V, CurDepth}, VP), [RC|NSC], [RC|AC]}
            end
        end,
        {ValidPaths, [], AvoidCells},
        NewPaths
    ),
    case dict:is_key(Destination, NValidPaths) of
        true -> NValidPaths;
        false ->
            % keep searching
            get_path_acc(Map, MapSize, NSeedCells, Destination, CurDepth + 1, NValidPaths, NAvoidCells)
    end
.

% Given a Dict of Paths and a destination position
% returns a list of cells from Origin to Destination
extract_path(PathDict, Destination) ->
    extract_path_acc(PathDict, Destination, [Destination])
.

extract_path_acc(PathDict, Destination, Path) ->
    case dict:fetch(Destination, PathDict) of
        {origin, 0} -> Path;
        {PrevCell, _Depth} ->
            extract_path_acc(PathDict, PrevCell, [PrevCell | Path])
    end
.
