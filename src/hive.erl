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
    start_link/4,
    stop/1,
    get_decision/4
]).

-record(state, {
    max_col,
    max_row,
    self_rc,
    my_ants_map,
    water_map,
    paths_dict,
    paths_turn
}).

-define(MAX_DEPTH, 40).     % path finding max depth
-define(REFRESH_RATE, 20).   % recalculate path every X turn

%%% %%% %%%
%% API
%%% %%% %%%

start_link(WaterMap, MyAntsMap, MapSize, RC) ->
    % TODO: change from start_link to start
    gen_server:start_link(?MODULE, [WaterMap, MyAntsMap, MapSize, RC], [])
.

stop(Pid) -> gen_server:cast(Pid, stop).

get_decision(Pid, From, AntsPid, Turn) ->
    gen_server:cast(Pid, {get_decision, From, AntsPid, Turn})
.

%%% %%% %%%
%% Internal functions
%%% %%% %%%



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
        paths_dict = undefined,
        paths_turn = 0
    },
    {ok, State}
.

handle_info({achieved, From, _}, S) ->
    error_logger:info_msg("Hive: razed by: ~p", [From]),
    {stop, normal, S}
;
handle_info(Msg, S) ->
    error_logger:info_msg("Hive: ?? msg: ~p", [Msg]),
    {noreply, S}
.

% find paths from the hive until we find ants.
% TODO: if water_map grows/changes we need to reset the paths_dict
% TODO: send move orders to ants in reach
handle_cast({get_decision, From, AntsPid, Turn}, S) ->
    WMap = S#state.water_map,
    AMap = S#state.my_ants_map,
    MapSize = {S#state.max_row, S#state.max_col},
    SelfRC = S#state.self_rc,
    RefreshTurn = S#state.paths_turn,
    % rebuild the paths based on REFRESH_RATE
    {NPathsDict, NRefreshTurn} = case
        ((Turn - RefreshTurn) >= ?REFRESH_RATE) or (S#state.paths_dict == undefined)
    of
        true ->
            PathsDict = pf:get_paths(WMap, MapSize, SelfRC, ?MAX_DEPTH),
            AntsCells = lists:foldl(
                fun(RC, Acc) ->
                    case proplists:lookup(RC, AntsPid) of
                        {RC, Pid} -> [{RC, Pid} | Acc];
                        none -> Acc
                    end
                end,
                [],
                dict:fetch_keys(PathsDict)
            ),
            lists:map(
                fun({RC, Pid}) ->
                    % Add hive location to the last movement of the path
                    P = lists:reverse([SelfRC | pf:extract_path(PathsDict, RC)]),
                    % Notify ants to follow path
                    ant:set_target(Pid, self(), SelfRC, P)
                end,
                AntsCells
            ),
            {PathsDict, Turn}
        ;
        false ->
            {S#state.paths_dict, RefreshTurn}
    end,
    {noreply, S#state{paths_dict=NPathsDict, paths_turn=NRefreshTurn}}
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
