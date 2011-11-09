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
    start_link/0,
    stop/0,
    get_decision/2
]).

-record(state, {
    last_col,
    last_row,
    dynamic_map,
    static_map
}).

start_link([StaticMap, DynamicMap]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [StaticMap, DynamicMap], [])
.

stop() -> gen_server:call(?MODULE, stop).

get_decision({R, C}, Turn) ->
    gen_server:cast(?MODULE, {get_decision, {R, C}, Turn})
.

random_direction() ->
    Dir = ["N", "S", "E", "W"],
    Nth = random:uniform(4),
    lists:nth(Nth, Dir)
.

%%% %%% %%%

init([StaticMap, DynamicMap]) ->
    DynMap = ets:new(dynamic_map, []),
    StaMap = ets:new(static_map, []),
    State = #state{dynamic_map = DynamicMap, static_map = StaticMap},
    {ok, State}
.

handle_info(_, S) ->
    {noreply, S}
.

handle_cast({get_decision, {R, C}, Turn}, S) ->
    random_direction(),
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
