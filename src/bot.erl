-module(bot).

-export([run/0]).

run() ->
    world:start_link(),
    Line = io:get_line(standard_io, ""),
    case Line of
        eof ->
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

% ["loadtime"]
    % in milliseconds
% ["turntime"]
    % in milliseconds
% ["rows"]
    % number of rows in the map
% ["cols"]
    % number of columns in the map
% ["turns"]
    % maximum number of turns in the game
% ["viewradius2"]
    % view radius squared
% ["attackradius2"]
    % battle radius squared
% ["spawnradius2"]
    % spawn radius squared
% ["player_seed"]
    % seed for random number generator, useful for reproducibility

parse_line(Line) ->
    Striped = string:strip(Line, right, $\n),
    Tokens = string:tokens(Striped, " "),
    case Tokens of
        ["ready"] ->
            io:format("go~n")
        ;
        ["d", R, C, O] -> % dead ants
            world:update_map(dead_ant, [
                list_to_integer(R),
                list_to_integer(C),
                list_to_integer(O)
            ])
        ;
        ["a", R, C, O] -> % ants
            world:update_map(ant, [
                list_to_integer(R),
                list_to_integer(C),
                list_to_integer(O)
            ]),
            % FIXME: remove this logic
            case O of
                "0" -> io:format("o ~s ~s ~s", [R, C, direction()]);
                _ -> nop
            end
        ;
        ["f", R, C] -> % food
            world:update_map(food, [list_to_integer(R), list_to_integer(C)])
        ;
        ["w", R, C] -> % water
            world:update_map(water, [list_to_integer(R), list_to_integer(C)])
        ;
        ["go"] ->
            io:format("go~n")
        ;
        [Setting, Value] ->
            world:set_variable(Setting, list_to_integer(Value))
        ;
        ["end"] ->
            % end game state
            nop
        ;
        ["score"|Scores] ->
            world:set_variable("score", Scores)
        ;
        _ -> % Ignore not implemented functionality
            nop
    end
.

direction() ->
    Dir = ["N", "S", "E", "W"],
    Nth = random:uniform(4),
    lists:nth(Nth, Dir)
.
