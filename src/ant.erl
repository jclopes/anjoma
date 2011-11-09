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
    start_link/1,
    stop/1,
    get_decision/3
]).

-record(state, {
    last_col,
    last_row,
    dynamic_map,
    static_map
}).

start_link(StaticMap) ->
    gen_server:start_link(?MODULE, [StaticMap], [])
.

stop(Pid) -> gen_server:call(Pid, stop).

get_decision(Pid, {R, C}, Turn) ->
    gen_server:cast(Pid, {get_decision, {R, C}, Turn})
.

random_direction() ->
    Dir = ["N", "S", "E", "W"],
    Nth = random:uniform(4),
    lists:nth(Nth, Dir)
.

%%% %%% %%%

init(StaticMap) ->
    State = #state{static_map = StaticMap},
    {ok, State}
.

handle_info(_, S) ->
    {noreply, S}
.

handle_cast({get_decision, {R, C}, Turn}, S) ->
    random_direction(),
    io:format("o ~w ~w ~s~n", [R, C, random_direction()]),
    % should return current position + direction + turn
    {noreply, S}
.


handle_call(stop, _, S) ->
    {stop, normal, S}
.

terminate(_Reason, _S) ->
    ok
.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}
.
