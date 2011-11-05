-module(world).

-behaviour(gen_server).

-export([
    init/1,
    handle_cast/2,
    terminate/2
]).

-export([
    start_link/0,
    stop/0,
    set_load_time/1
]).
-record(game_settings, {load_time, turn_time, lines, columns}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, [])
.

stop() -> gen_server:call(?MODULE, stop).

set_load_time(LoadTime) ->
    gen_server:cast(?MODULE, {load_time, LoadTime})
.

%%% %%% %%%

init(no_args) ->
    {ok, #game_settings{}}
.

handle_cast({load_time, LoadTime}, S) ->
    NewS = S#game_settings{load_time = LoadTime},
    {noreply, NewS}
.

handle_call(stop, _, S) ->
    {stop, normal, S}
.

terminate(Reason, S) ->
    ok
.