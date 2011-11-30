-module(hive).

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
    self_rc,
    my_ants_map,
    water_map,
    paths_dict
}).

-define(MAX_DEPTH, 30).

%%% %%% %%%
%% API
%%% %%% %%%

start_link(WaterMap, MyAntsMap, MapSize) ->
    % TODO: change from start_link to start
    gen_server:start_link(?MODULE, [WaterMap, MyAntsMap, MapSize], [])
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

food_path(WaterMap, MyAntsMap, MapSize, StartPos) ->
    % TODO: search depth should be smaller or equal to the viewing range
    PathDict = pf:get_paths(WaterMap, MapSize, StartPos, ?MAX_DEPTH),
    case dict:size(PathDict) of
        1 -> [StartPos, StartPos]; % There is no possible move. Stay put.
        _ ->
            FoodCells = lists:filter(
                fun(RC) ->
                    ets:member(MyAntsMap, pf:coord_to_pos(MapSize, RC))
                end,
                dict:fetch_keys(PathDict)
            ),
            case FoodCells of
                [] -> % no food go to most distant direction
                    {ListDistantRC, _} = dict:fold(
                        fun(K, {_, Depth}, {[MK|Tail], MaxDepth}=Acc) ->
                            case Depth of
                                D when D > MaxDepth -> {[K], D};
                                D when D == MaxDepth -> {[K, MK|Tail], D};
                                _ -> Acc
                            end
                        end,
                        {[undefined], -1},
                        PathDict
                    ),
                    pf:extract_path(PathDict, random_element(ListDistantRC))
                ;
                [F|_] -> pf:extract_path(PathDict, F)
            end
    end
.

random_element(List) ->
    Size = length(List),
    lists:nth(random:uniform(Size), List)
.

%%% %%% %%%
%% gen_server API
%%% %%% %%%

init([WaterMap, MyAntsMap, {MaxRow, MaxCol}, RC]) ->
    {R1,R2,R3} = now(),
    random:seed(R1,R2,R3),
    State = #state{
        my_ants_map = MyAntsMap,
        water_map = WaterMap,
        max_row = MaxRow,
        max_col = MaxCol,
        self_rc = RC,
        paths_dict = undefined
    },
    {ok, State}
.

handle_info(_, S) ->
    {noreply, S}
.

% find paths from the hive until we find ants.
% TODO: if water_map grows/changes we need to reset the paths_dict
% TODO: send move orders to ants in reach
handle_cast({get_decision, From, Turn}, S) ->
    WMap = S#state.water_map,
    AMap = S#state.my_ants_map,
    MapSize = {S#state.max_row, S#state.max_col},
%    PathsDict = S#state.paths_dict,

%    NPathsDict = case PathsDict of
%        undefined ->
%            find_ants(WMap, AMap, MapSize, S#state.self_rc),
%        _ ->
%            expand_paths(WMap, AMap, MapSize, PathsDict)
%    end,
%    NState = S#state{current_path = NPath},
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
