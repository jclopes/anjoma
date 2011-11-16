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

expand_cell_acc(MapStatic, MapDynamic, MapSize, [H | StartPoss], PathDict, 0) ->
    % return random path
    {PathDict, H}
;
expand_cell_acc(MapStatic, MapDynamic, {MaxRow, MaxCol}=MapSize, StartPoss, PathDict, MaxDepth) ->
    DictExpCells = expand_cell_list_acc(MapSize, StartPoss, PathDict),
    ExpCells = dict:fetch_keys(DictExpCells),
    % remove Cells that are water
    NoWaterCells = lists:foldl(
        fun(RC, Acc) ->
            case ets:member(MapStatic, coord_to_pos(RC, MaxCol)) of
                true -> dict:erase(RC, Acc);
                false -> Acc
            end
        end,
        ExpCells
    ),
    
    % check if we found food
    FoodCells = lists:filter(
        fun(RC) ->
            case ets:lookup(MapStatic, coord_to_pos(RC, MaxCol)) of
                {Pos, food, _Turn} -> true;
                _ -> false
            end
        end,
        dict:fetch_keys(NoWaterCells)
    ),
    case FoodCells of
        [] ->
        %    expand next level
            expand_cell_acc(MapStatic, MapDynamic, MapSize, ExpCells, DictExpCells, MaxDepth - 1)
        ;
        [FoodCell|_] ->
        %    get the path from the origin
            {DictExpCells, FoodCell}
    end
.

% Given a MapSize and a list of starting positions,
% get all the adjacent cells to those starting positions.
% Returns a dict that contains elements in the format {Cell, StartingCell}.
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
    MaxCol = S#state.max_col,
    Pos = R*MaxCol + C,
    {NPos, {DR, DC}, D} = random_direction(SMap, MaxCol, R, C),
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
