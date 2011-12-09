% MEMORY_LIMIT=1500Mb
% erl -hms"+ str(MEMORY_LIMIT) +"m -smp disable -noshell -s my_bot start -s init stop

-module(my_bot).

% TODO: make this a proper application or supervisor

%-behaviour(application).
%-export([start/2, stop/1]).

-export([start/0]).

start() ->
    error_logger:tty(false),
%    error_logger:logfile({open, 'bot.log'}),
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
            start()
    end
.

parse_line(Line) ->
    Striped = string:strip(Line, right, $\n),
    Tokens = string:tokens(Striped, " "),
%    error_logger:info_msg("input: ~s", [Striped]),
    case Tokens of
        ["ready"] ->
            io:format("go~n")
        ;
%        ["d", R, C, "0"] -> % our dead ants - ignore others
%            world:update_map(dead_ant, [
%                list_to_integer(R),
%                list_to_integer(C),
%                0
%            ])
%        ;
        ["a", R, C, O] -> % ants
            world:update_map(ant, [
                list_to_integer(R),
                list_to_integer(C),
                list_to_integer(O)
            ])
        ;
        ["h", R, C, O] -> % ants
            world:update_map(hill, [
                list_to_integer(R),
                list_to_integer(C),
                list_to_integer(O)
            ])
        ;
        ["f", R, C] -> % food
            world:update_map(food, [list_to_integer(R), list_to_integer(C)])
        ;
        ["w", R, C] -> % water
            world:update_map(water, [list_to_integer(R), list_to_integer(C)])
        ;
        ["go"] ->
            world:go()
        ;
        [Setting, Value] ->
            world:set_variable(Setting, list_to_integer(Value))
        ;
        ["end"] ->
            % end game state
            world:dump_log()
        ;
        ["score"|Scores] ->
            world:set_variable("score", Scores)
        ;
        _ -> % Ignore not implemented functionality
            nop
    end
.
