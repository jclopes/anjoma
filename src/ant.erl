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
    start_link/2,
    stop/1,
    get_decision/4
]).

-record(state, {
    last_col,
    last_row,
    dynamic_map,
    static_map
}).

start_link(StaticMap, DynamicMap) ->
    gen_server:start_link(?MODULE, [StaticMap, DynamicMap], [])
.

stop(Pid) -> gen_server:cast(Pid, stop).

get_decision(Pid, From, {R, C}, Turn) ->
    gen_server:cast(Pid, {get_decision, From, {R, C}, Turn})
.

random_direction(R, C) ->
    Nth = random:uniform(4),
    case Nth of
        1 ->
            {R - 1, C, "N"}
        ;
        2 ->
            {R + 1, C, "S"}
        ;
        3 ->
            {R, C + 1, "E"}
        ;
        4 ->
            {R, C - 1, "W"}
    end
.

%%% %%% %%%

init([StaticMap, DynamicMap]) ->
    State = #state{dynamic_map = DynamicMap, static_map = StaticMap},
    {ok, State}
.

handle_info(_, S) ->
    {noreply, S}
.

handle_cast({get_decision, From, {R, C}, Turn}, S) ->
    {DR, DC, D} = random_direction(R, C),
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
