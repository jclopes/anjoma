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
    N = {(R - 1)*MaxCol+C, {R - 1, C}, "N"},
    S = {(R + 1)*MaxCol+C, {R + 1, C}, "S"},
    E = {R*MaxCol + C + 1, {R, C + 1}, "E"},
    W = {R*MaxCol + C - 1, {R, C - 1}, "W"},
    Dirs = [N,S,E,W],
    ValidDirs = lists:filter(
        fun({Pos, _RC, _D}) ->
            not ets:member(Map, Pos)
        end,
        Dirs
    ),
    Nth = random:uniform(length(ValidDirs)),
    lists:nth(Nth, ValidDirs)
.

coord_to_pos(R, C, MaxCol) ->
    R*MaxCol + C
.

opposite_direction("N") -> "S";
opposite_direction("S") -> "N";
opposite_direction("E") -> "W";
opposite_direction("W") -> "E".

get_NSEW(R,C,MaxCol) ->
    N = {(R - 1)*MaxCol+C, {R - 1, C}, "N"},
    S = {(R + 1)*MaxCol+C, {R + 1, C}, "S"},
    E = {R*MaxCol + C + 1, {R, C + 1}, "E"},
    W = {R*MaxCol + C - 1, {R, C - 1}, "W"},
    [N,S,E,W]
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
        get_NSEW(R, C, MaxCol)
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
