%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : atm.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2011 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ant_food).
-behaviour(gen_fsm).

-export([start/1, start_link/1, stop/1, provide_move/1]).

-export([init/1, give_move/2, give_move/3, find_path/2,
         handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {name, cur_pos, path = []}).

-define(TIME_LIMIT, 10000).

%%% USER API
start(Name) -> gen_fsm:start({local, {?MODULE, Name}}, ?MODULE, Name, []).

%start_link(Name, Map, Pos)
start_link({Name, Map, Pos}) ->
    gen_fsm:start_link({local, {?MODULE, Name}}, 
		       ?MODULE, 
		       {Name, Map, Pos}, 
		       []).

stop(Name) -> gen_fsm:send_event({local, {?MODULE, Name}}, stop).

%provide_move(Name, Map, Position)
%get_move
provide_move({Name, Map, Pos}) ->
    gen_fsm:sync_send_event({local, {?MODULE, Name}},
			    {provide_move, Map, Pos}).


%%% EXPORTED FOR GEN_FSM
init({Name, _Map, Pos}) ->
    {ok, give_move, #state{name = Name, cur_pos = Pos}}.

% if path ready output move
give_move({provide_move, _Map, Pos}, 
	  _From,
	  #state{path = Path} = S) 
  when Path =/= [] ->
  
    [NewPos,NextNew|Tail] = Path,

    %if position is the same, then re-output otherwise pop one
    case NewPos == Pos of
	false ->
	    {reply, NewPos, give_move, S};
	true ->
	    {reply, 
	     NextNew, 
	     give_move, 
	     S#state{cur_pos = Pos, path = [NextNew|Tail]}}
    end;
% else find path
give_move({provide_move, Map, Pos}, _From, #state{name = Name} = S) ->
    gen_fsm:send_event({local, {?MODULE, Name}}, {find_path, Map, Pos}),
    {reply, Pos, give_move, S}.

    % synchronous version
    %Path = find_path(Map, Pos),
    %{reply, Pos, give_move, S#state{path = Path}}.

give_move(stop, State) -> {stop, normal, State}.


%find_path({})
% path finding
% get list of food
find_path({find_path, _Map, _Pos}, _S) ->
    ok.
    %FoodList = ets:lookup(Map, {Pos, water, _}).
% check for absolute distance and take the minimum

% take the minimum
% create a path

handle_event(Event, _, State) ->
    {stop, {"Can not handle event in state", Event}, State}.

handle_sync_event(Event, _, _StateName, State) ->
    {stop, {"Can not handle sync event", Event}, State}.

handle_info(Info, _, State) ->
    {stop, {"Can not handle info", Info}, State}.

code_change(_, StateName, State, _) -> {ok, StateName, State}.

terminate(_, _, _) -> ok.

