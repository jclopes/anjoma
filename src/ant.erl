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
    get_decision/4
]).

-record(state, {
    max_col,
    max_row,
    dynamic_map,
    static_map
}).

start_link(StaticMap, DynamicMap, MaxRow, MaxCol) ->
    gen_server:start_link(?MODULE, [StaticMap, DynamicMap, MaxRow, MaxCol], [])
.

stop(Pid) -> gen_server:cast(Pid, stop).

get_decision(Pid, From, {R, C}, Turn) ->
    gen_server:cast(Pid, {get_decision, From, {R, C}, Turn})
.

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
    MaxCols = S#state.max_col,
    {NPos, {DR, DC}, D} = random_direction(SMap, MaxCols, R, C),
    % TODO: send the Pos and NPos : {Pos, R, C}, {NPos, DR, DC}
    From ! {move, self(), {R, C}, {DR, DC}, D, Turn},
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
