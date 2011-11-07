% MEMORY_LIMIT=1500Mb
% erl -hms"+ str(MEMORY_LIMIT) +"m -smp disable -noshell -s my_bot start -s init stop

-module(my_bot).

-export([start/0]).

start() ->
    bot:run()
.
