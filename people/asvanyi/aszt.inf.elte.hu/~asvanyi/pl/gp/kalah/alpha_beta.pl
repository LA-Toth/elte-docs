% General framework

:- module( alpha_beta, [ init_alpha_beta/1, game/0, next_player/2 ] ).

init_alpha_beta(Module) :-
    use_module( Module, [ init_game/2, show_position/3,
			  game_over/3, announce_result/1,
                          make_move/3, swap_position/2,
                          lookahead/1, alpha_beta_limits/2, read_move/2, 
                          possible_move/2, value_of_position/2 ] ).
 
game :- 
    init_game(Player,Position),
    show_position(Player,Position,initialization),
    play(Player,Position).

play(Player,Position) :-
    ( game_over(Position,Player,Result) ->
          announce_result(Result)
    ; choose_move(Player,Position,Move),
      make_move(Move,Position,Position1),
      show_position(Player,Position1,Move),
      next_player(Player,Player1),
      swap_position(Position1,Position11),
      play(Player1,Position11)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicates on two-player games:

choose_move(computer,Position,Move) :-
    lookahead(Depth), alpha_beta_limits(Alpha,Beta),
    alpha_beta(Depth,Position,Alpha,Beta,Move,_Value).
choose_move(opponent,Position,Move) :-
    read_move(Position,Move).

next_player(computer,opponent).
next_player(opponent,computer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is the alpha-beta algorithm.

alpha_beta(Depth,Position,Aplha,Beta,Move,Value) :-
    Depth>0,
    findall( Move_from_Position,
	     possible_move(Position,Move_from_Position),
	     Moves
	   ),
    Depth1 is Depth-1, Alpha1 is -Beta, Beta1 is -Aplha,
    evaluate_and_choose(Moves,Depth1,Position,void,Alpha1,Beta1,
                        Move,Value).
alpha_beta(0,Position,_,_,_,Value) :-
    value_of_position(Position,Value).

evaluate_and_choose([Move1|Moves],Depth,Position,MoveAlpha,Alpha,Beta,
                    Move,Value) :-
    make_move(Move1,Position,Position1),
    swap_position(Position1,Position11),
    alpha_beta(Depth,Position11,Alpha,Beta,_,NegatedValue),
    Value1 is -NegatedValue,
    cut_off(Move1,Value1,Moves,Depth,Position,MoveAlpha,Alpha,Beta,
            Move,Value).
evaluate_and_choose([],_Depth,_Position,Move,Alpha,_Beta,Move,Alpha).

cut_off(Move1,Value1,Moves,Depth,Position,MoveAlpha,Alpha,Beta,Move,Value) :-
    ( Value1 >= Beta -> Move = Move1, Value = Value1
    ; Value1 =< Alpha ->
      evaluate_and_choose(Moves,Depth,Position,MoveAlpha,Alpha,Beta,Move,Value)
    ; evaluate_and_choose(Moves,Depth,Position,Move1,Value1,Beta,Move,Value)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
