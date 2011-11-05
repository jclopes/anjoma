-module(bot).

-export([run/0]).

run() ->
    world:start_link(),
    Line = io:get_line(standard_io, ""),
    case Line of
        eof ->
%            io:format("Finished~n"),
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
        ["ready"] ->
            io:format("go~n")
        ;
%        ["turn", _Turn] ->
%            world:set_load_time(100),
%            io:format("go~n")
%        ;
        ["a", R, C, O] ->
            case O of
                "0" ->
                    D = direction(),
                    io:format("o ~s ~s ~s~n", [R,C,D])
                ;
                _ ->
%                    io:format("enemy ant~n"),
                    nop
            end
        ;
        ["go"] ->
            io:format("go~n")
        ;
        _ ->
            nop
    end
.

direction() ->
    Dir = ["N", "S", "E", "W"],
    Nth = random:uniform(4),
    lists:nth(Nth, Dir)
.