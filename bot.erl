-module(bot).

-export([run/0]).

run() ->
    world:start_link(),
    Line = io:get_line(standard_io, ""),
    case Line of
        eof ->
            io:format("Finished~n"),
            ok
        ;
        {error, Reason} ->
            io:format("Error reading from STDIO: ~p~n", [Reason]),
            ko
        ;
        Data ->
            parse_line(Data),
            run()
    end
.

parse_line(Line) ->
    Striped = string:strip(Line, right, $\n),
    Tokens = string:tokens(Striped, " "),
    case Tokens of
        ["turn", Turn] ->
            io:format("Turn Data:~n=> ~p~n", [Turn]),
            world:set_load_time(100)
        
    end
.
