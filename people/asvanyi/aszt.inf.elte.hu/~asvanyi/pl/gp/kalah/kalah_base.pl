% This is a module for playing a two-player game called 'kalah'.

:- module( kalah_base, [ init_kalah/1,
			 kalah/0,
                         init_game/2, show_position/3,
			 game_over/3, announce_result/1,
                         make_move/3, swap_position/2,
                         lookahead/1, alpha_beta_limits/2, read_move/2, 
                         possible_move/2, value_of_position/2
                       ]
	 ).

:- use_module( kalah_utilities, [ lookahead/1, kalah_help/0 ] ).
:- use_module( distribute_stones, [ distribute_stones/4 ] ).
:- use_module( library(lists), [ reverse/2, nth1/3 ] ).
:- use_module( env3, [ yesno/3, type_seq/1, tab/2, get/2 ] ).

init_kalah(Module) :- use_module( Module, [ game/0, next_player/2 ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kalah :- catch( ( kalah_help, game ), 
                stop_kalah,
		( write(user,'Good-bye!'), nl(user) )
              ).

% Predicates for game/0

init_game(Player,board([N,N,N,N,N,N],0,[N,N,N,N,N,N],0)) :-
    pieces(N),
    yesno('Do You want to start play?', Player=opponent, Player=computer ).

pieces(6).

show_position(opponent,Position,Move) :-
    type_seq( [ nl, 'Your turn: ', Move, nl, nl ] ),
    swap_position(Position,Position1),
    show(Position1), nl.
show_position(computer,Position,Move) :-
    type_seq( [ nl, 'My turn: ', Move, nl, nl ] ),
    show(Position), nl(user).


swap_position( board(Holes1,Kalah1,Holes2,Kalah2),
	       board(Holes2,Kalah2,Holes1,Kalah1)
	     ).
 
show(board(Holes1,Kalah1,Holes2,Kalah2)) :-
    reverse(Holes1,Holes1R),
    write_stones(Holes1R),
    write_kalahs(Kalah1,Kalah2),
    write_stones(Holes2).

write_stones(H) :- tab(user,3), display_holes(H), nl(user).

display_holes([H|Hs]) :-
    write_pile(H), display_holes(Hs).
display_holes([]).

write_pile(N) :-
    ( N<10 -> tab(user,4)
    ; N<100 -> tab(user,3)
    ; tab(user,2)
    ),
    write(user,N).

write_kalahs(K1,K2) :-
    ( K1<10 -> tab(user,2)
    ; K1<100 -> tab(user,1)
    ; true
    ),
    type_seq( [ K1, tab(34), K2, nl ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicates for play/2 

game_over(board(_,N,_,N),_,draw) :- pieces(K), N=:=6*K, !.
game_over(board(_,KalahPlayer,_,_),Player,Player) :-
    pieces(N), KalahPlayer>6*N, !.
game_over(board(_,_,_,KalahOpponent),Player,Opponent) :-
    pieces(N), KalahOpponent>6*N, %!,
    next_player(Player,Opponent).

announce_result(draw) :- write(user,'The game is a draw.'), nl(user).
announce_result(computer) :- write(user,'I won.'), nl(user).
announce_result(opponent) :- write(user,' You Won! Congratulations.'),nl(user).

% choose_move(Player,Position,Move) <- See 'alpha_beta.pl'.

% make_move(Move,Position,Position1) <- 
%     See the predicates for evaluate_and_choose/8 bellow.

% show_position(Player,Position1) <- See the preds. for game/0 above.

% next_player(Player,Player1) <- See 'alpha_beta.pl'.

% swap_position(Position1,Position11) <- See the preds. for game/0 above.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicates for choose_move/3

alpha_beta_limits(Alpha,Beta) :- pieces(N), Beta is 12*N+1, Alpha is -Beta.

% alpha_beta(Depth,Position,Alpha,Beta,Move,_) <- See 'alpha_beta.pl'.

read_move(Position,[Move|Moves]) :-
  repeat,
    type_seq( [ 'Type in the sequence number (1-6) of a (nonempty) hole', nl,
	      'containing the stones to be picked up (or h for help), please.',
              nl, 'h or Sequence_Number = ', flush ] ), 
    get(user,MoveChar), skip_line(user), 
    ( ( MoveChar==0'h ; MoveChar==0'H ) -> 
	kalah_help,
	show_position(opponent,Position,repeated),
	fail
    ; Move is MoveChar - 48, legal_move(Position,Move,Stones)
    ),
  !,
  extend_read_move(Position,Move,Stones,Moves).

legal_move(Board,Move,Stones) :- 
    1 =< Move, Move =< 6,
    stones_in_hole(Move,Board,Stones).    % defined in the next section

extend_read_move(Position,Move,Stones,Moves) :-
    ( 7-Move =:= Stones mod 13,
      distribute_stones(Stones,Move,Position,Position1),
      \+ empty_holes(Position1) ->
	show_position(opponent,Position1,[Move]),
	type_seq( [ 'You have another turn.', nl, nl ] ),
	read_move(Position1,Moves)
    ; Moves = []
    ).

% distribute_stones/4 is defined in its own file.

empty_holes(board([0,0,0,0,0,0],_,_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicates for alpha_beta/6 and for evaluate_and_choose/8

possible_move(board([0,0,0,0,0,0],_,_,_),[]) :- !.
possible_move(Board,[Move|Moves]) :-
    member(Move,[1,2,3,4,5,6]),
    stones_in_hole(Move,Board,Stones),
    extend_move(Stones,Move,Board,Moves).

stones_in_hole(Move,board(MyHoles,_,_,_),Stones) :-
    nth1(Move,MyHoles,Stones), Stones>0.

extend_move(Stones,Move,Board,Moves) :-
    ( 7-Move =:= Stones mod 13 ->
	distribute_stones(Stones,Move,Board,Board1), % defined in its own file
	possible_move(Board1,Moves)
    ; Moves = []
    ).

value_of_position(board(_,MyKalah,_,YourKalah),Value) :-
    Value is MyKalah-YourKalah.

make_move([Move|Moves],Board,FinalBoard) :-
    stones_in_hole(Move,Board,Stones),
    distribute_stones(Stones,Move,Board,Board1), % defined in its own file
    make_move(Moves,Board1,FinalBoard).
make_move([],Board,Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
