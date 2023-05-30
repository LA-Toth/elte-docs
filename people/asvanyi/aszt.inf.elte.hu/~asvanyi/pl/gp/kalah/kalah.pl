% Main program of the game called 'Kalah'.
% The game is to be started typing '?- kalah.' at the Prolog prompt.

:- module( kalah, [ kalah/0, optimize/0 ] ).

%%% :- use_module( alpha_beta, [ init_alpha_beta/1 ] ).

%%% :- use_module( kalah_base, [ init_kalah/1, kalah/0 ] ).

%%% :- init_alpha_beta(kalah_base), init_kalah(alpha_beta).

:- write( user, 'To start, type ''?- kalah.'' at the Prolog prompt.' ),
   nl(user).

%%% optimize :-
%%% 	fcompile( [ alpha_beta, distribute_stones, kalah_utilities,
%%% 		        kalah_base, kalah
%%% 		      ]
%%% 		    ),
%%% 	use_module(kalah).

%%% General framework

%%% :- module( alpha_beta, [ init_alpha_beta/1, game/0, next_player/2 ] ).

%%% init_alpha_beta(Module) :-
%%%     use_module( Module, [ init_game/2, show_position/3,
%%% 			  game_over/3, announce_result/1,
%%%                           make_move/3, swap_position/2,
%%%                           lookahead/1, alpha_beta_limits/2, read_move/2, 
%%%                           possible_move/2, value_of_position/2 ] ).
 
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

%%% % This is a module for playing a two-player game called 'kalah'.

%%% :- module( kalah_base, [ init_kalah/1,
%%% 			 kalah/0,
%%%                          init_game/2, show_position/3,
%%% 			 game_over/3, announce_result/1,
%%%                          make_move/3, swap_position/2,
%%%                          lookahead/1, alpha_beta_limits/2, read_move/2, 
%%%                          possible_move/2, value_of_position/2
%%%                        ]
%%% 	 ).

%%% :- use_module( kalah_utilities, [ lookahead/1, kalah_help/0 ] ).
%%% :- use_module( distribute_stones, [ distribute_stones/4 ] ).
:- use_module( library(lists), [ reverse/2, nth1/3 ] ).
%%% :- use_module( env3, [ yesno/3, type_seq/1, tab/2, get/2 ] ).

%%% init_kalah(Module) :- use_module( Module, [ game/0, next_player/2 ] ).

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

%%% % This file is needed for kalah.pl .
%%% % Its content:
%%% % Clauses and predicates for distribute_stones/4

%%% :- module( ditribute_stones, [distribute_stones/4] ).

%%% :- use_module( library(lists), [nth1/3,sumlist/2] ).
:- use_module( library(lists), [sumlist/2] ).

distribute_stones(Stones,Hole,Board,FinalBoard) :-
    first_distribute_my_holes(Stones,Hole,Board,Board1,Stones1),
    distribute_your_holes(Stones1,Board1,Board2),
    check_if_finished(Board2,FinalBoard).

first_distribute_my_holes(Stones,Hole,
                          board(MyHoles,MyKalah,YourHoles,YourKalah),
                          board(MyHoles1,MyKalah1,YourHoles,YourKalah),
                          Stones1) :-
    Stones>=7-Hole, !,
    pick_up_and_distribute(Hole,Stones,MyHoles,MyHoles1),
    MyKalah1 is MyKalah+1,
    Stones1 is Stones+Hole-7.
first_distribute_my_holes(Stones,Hole,
                          board(MyHoles,MyKalah,YourHoles,YourKalah),
                          board(MyHoles1,MyKalah1,YourHoles1,YourKalah),0) :-
    % Stones<7-Hole, 
    FinishingHole is Hole+Stones,
    nth1(FinishingHole,MyHoles,0),
    OppositeHole is 7-FinishingHole,
    nth1(OppositeHole,YourHoles,Y),
    Y>0, !,
    Stones1 is Stones-1,
    pick_up_and_distribute(Hole,Stones1,MyHoles,MyHoles1),
    nth_substitute(OppositeHole,YourHoles,0,YourHoles1),
    MyKalah1 is MyKalah+Y+1.
first_distribute_my_holes(Stones,Hole,
                          board(MyHoles,MyKalah,YourHoles,YourKalah),
                          board(MyHoles1,MyKalah,YourHoles,YourKalah),0) :-
    /* Stones<7-Hole,
        \+ ( FinishingHole is Hole+Stones,
             nth1(FinishingHole,MyHoles,0),
             OppositeHole is 7-FinishingHole,
             nth1(OppositeHole,YourHoles,Y),
             Y>0
           ), 
       !,
    */
    pick_up_and_distribute(Hole,Stones,MyHoles,MyHoles1).

pick_up_and_distribute(H,Stones,[Hole|Holes],[Hole|Holes1]) :-
    H>1, !,
    H1 is H-1, pick_up_and_distribute(H1,Stones,Holes,Holes1).
pick_up_and_distribute(1,Stones,[_|Holes],[0|Holes1]) :-
    distribute(Stones,Holes,Holes1).
    
distribute(H,[Hole|Holes],[Hole1|Holes1]) :-
    H>0, !,
    H1 is H-1, Hole1 is Hole+1,
    distribute(H1,Holes,Holes1).
distribute(0,Holes,Holes) :- !.
distribute(_,[],[]).

nth_substitute(N,[Hole|Holes],NewHole,[Hole|NewHoles]) :-
    N>1, !,
    N1 is N-1, nth_substitute(N1,Holes,NewHole,NewHoles).
nth_substitute(1,[_|Holes],NewHole,[NewHole|Holes]).

distribute_your_holes(0,Board,Board) :- !.
distribute_your_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                      board(MyHoles,MyKalah,YourHoles1,YourKalah)) :-
    /* 1=<Stones, */ Stones=<6, !,
    distribute(Stones,YourHoles,YourHoles1).
distribute_your_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                      FinalBoard) :-
    /* Stones>6, !, */
    distribute(Stones,YourHoles,YourHoles1),
    Stones1 is Stones-6,
    distribute_remaining_stones(Stones1,
                                board(MyHoles,MyKalah,YourHoles1,YourKalah),
                                FinalBoard).

 distribute_remaining_stones(Stones,Board,FinalBoard) :-
    distribute_my_holes(Stones,Board,Board1,Stones1),
    distribute_your_holes(Stones1,Board1,FinalBoard).

distribute_my_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                    board(MyHoles1,MyKalah1,YourHoles,YourKalah), Stones1) :-
    Stones>=7, !,
    distribute(Stones,MyHoles,MyHoles1),
    MyKalah1 is MyKalah+1,
    Stones1 is Stones-7.
distribute_my_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                    board(MyHoles1,MyKalah1,YourHoles1,YourKalah), 0) :-
    /* Stones<7, */
    nth1(Stones,MyHoles,0),
    OppositeHole is 7-Stones,
    nth1(OppositeHole,YourHoles,Y),
    Y>0, !,
    Stones1 is Stones-1,
    distribute(Stones1,MyHoles,MyHoles1),
    nth_substitute(OppositeHole,YourHoles,0,YourHoles1),
    MyKalah1 is MyKalah+Y+1.
distribute_my_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                    board(MyHoles1,MyKalah,YourHoles,YourKalah), 0) :-
    /* Stones<7,
        \+ ( nth1(Stones,MyHoles,0),
             OppositeHole is 7-Stones,
             nth1(OppositeHole,YourHoles,Y),
             Y>0
           ), 
       !,
    */
    distribute(Stones,MyHoles,MyHoles1).

check_if_finished(board(MyHoles,MyKalah,YourHoles,YourKalah),
                  board(MyHoles,MyKalah,YourHoles1,YourKalah1) ) :-
    zero(MyHoles), !,
    sumlist(YourHoles,Sum), zero(YourHoles1), YourKalah1 is YourKalah+Sum.
check_if_finished(board(MyHoles,MyKalah,YourHoles,YourKalah),
                  board(MyHoles1,MyKalah1,YourHoles,YourKalah) ) :-
    zero(YourHoles), !,
    sumlist(MyHoles,Sum), zero(MyHoles1), MyKalah1 is MyKalah+Sum.
check_if_finished(Board,Board).

zero([0,0,0,0,0,0]).

%%% :- module( kalah_utilities, [ lookahead/1, kalah_help/0 ] ).

%%% :- use_module( env3, [ type_seq/1, ttyget_line/1 ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic lookahead/1.
 
lookahead(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kalah_help :- 
    repeat,
         lookahead(Level),
         type_seq( [ nl, 'Please,type', nl,
		     '  "s" to "stop" playing Kalah,', nl,
		     '  "r" to read the "rules" of the game,', nl,
		     '  "l" to change your "level" (1-4).', nl,	     
		     tab(3), Level, 
		     ' is your current level. (4 is the highest one.)', nl,
		     'Press <Enter> to continue playing Kalah.', nl,
		     'Your answer: ', flush ] ),
	ttyget_line(L), nl(user),
	( (L=[0's|_];L=[0'S|_]) -> throw(stop_kalah)
	; (L=[0'r|_];L=[0'R|_]) -> write_rules, fail
	; (L=[0'l|_];L=[0'L|_]) -> change_level, fail
	; L=[] -> true
	),
    !.

write_rules :-
    type_seq( [
'    Kalah is played on a board with two rows of six holes facing each other.',
 nl,
'Each player owns a row of six holes, plus a kalah to the right of the holes.',
 nl,
'In the initial state there are six stones in each hole and the two kalahs are'
, nl, 'empty, as follows:', nl,nl,
'                 6   6   6   6   6   6', nl,
'             0                           0', nl,
'                 6   6   6   6   6   6', nl, nl,
'    A player begins his move by picking up the stones of one of his holes.', 
 nl,
'Proceeding counterclockwise around the board, he puts one of the picked-up', 
 nl,
'stones in each hole and in his own kalah skipping the opponent''s kalah, ',
'until', nl,
'no stones remain to be distributed. There are three possible outcomes. ',
'If the', nl,
'last stone lands on the kalah, the player has another move. If the last stone'
, nl,
'lands on an empty hole owned by the player, and the opponent''s hole directly'
, nl,
'accross the board contains at least one stone, the player takes all the stones'
, nl,
'in the hole plus his last landed stone and puts them all in his kalah.', nl,
'Otherwise the player''s turn ends, and his opponent moves.', nl, nl,
'    If all the holes of a player becomes empty (even if it is not his turn to'
, nl,
'play), the stones remaining in the holes of the opponent are put into the', 
 nl,
'opponent''s kalah and the game ends. The winner of the game is the first', nl,
'player to get more than half of the stones in his kalah.', nl, nl,
'Press <Enter> to continue.', flush ] ),
    skip_line(user).


change_level :-
    repeat,
        write(user,'Your new level = '), flush_output(user),
        get(user,LevelChar), skip_line(user), Level is LevelChar-48, 
        ( 1 =< Level, Level =< 4 -> true
        ; write( user,
		 'Your new level should have been an integer between 1 and 4'),
          nl(user), fail
        ),
    !,
    retractall(lookahead(_)),
    asserta(lookahead(Level)).

%%% env3

get(S,C) :-
    get_code(S,D),
    ( D > (0' ) -> C = D
    ; get(S,C)
    ).

% ttyget_line(Chars) :-
%     Get a line from the standard input. 
%     Chars is the list of the corresponding character codes. (ASCII)
ttyget_line(Chars) :- get_line(user,Chars).

% get_line(Stream,Chars) :-
%     Get a line from Stream. 
%     Chars is the list of the corresponding character codes. (ASCII)
get_line(Stream,Chars) :-
	( at_end_of_stream(Stream) -> Chars=[]
	; at_end_of_line(Stream) -> skip_line(Stream), Chars=[]
	; get_code(Stream,C), Chars=[C|Cs], get_line(Stream,Cs)
        ).

% type_seq(Xs) :- 
%     Type the elements of list Xs on the standard output. 
%     Some terms are specially handled:
%     nl, flush, and tab(N)
%     raise the corresponding actions on the standard output.
type_seq(Xs) :- write_seq(user,Xs).

% write_seq(S,L) :- 
%     Write the elements of list L onto the stream S. 
%     Some terms are specially handled:
%     nl, flush, and tab(N) raise the corresponding actions on S.
write_seq(S,X) :- var(X), !, write(S,X).
write_seq(S,[X|Xs]) :- var(X), !, write(S,X), write_seq(S,Xs).
write_seq(S,[nl|Xs]) :- !, nl(S), write_seq(S,Xs).
write_seq(S,[flush|Xs]) :- !, flush_output(S), write_seq(S,Xs).
write_seq(S,[tab(N)|Xs]) :- !, tab(S,N), write_seq(S,Xs).
write_seq(S,[X|Xs]) :- !, write(S,X), write_seq(S,Xs).
write_seq(_S,[]) :- !.
write_seq(S,X) :- write(S,X).

tab(S,N) :- ( N > 0 -> put_code(S,0' ), N1 is N-1, tab(S,N1)
	    ; true
	    ).

% yesno( Question, YesProgram, NoProgram ) :-
%     It types Question using type_seq/1, and ' [yes] ' into the current line. 
%     If the user replies an empty line, or a line starting
%     with 'y' or 'Y', it performs 'YesProgram'.
%     If the user replies a line starting with 'n' or 'N',
%     it performs 'NoProgram'.
yesno( Question, YesProgram, NoProgram ) :-
	type_seq(Question), write(user,' [yes] '), flush_output(user),
	get_answer(Answer), 
	( Answer == 0'y -> YesProgram
	; Answer == 0'n -> NoProgram
	; write(user,'Wrong answer.'), nl(user),
	    yesno( Question, YesProgram, NoProgram )
	).

get_answer(Answer) :-
	peek_code(user,C),
	( C == 10 -> skip_line(user), Answer = 0'y    % Linux eoln
	; C == 13 -> skip_line(user), Answer = 0'y    % dos eoln
	; C == 0'Y -> skip_line(user), Answer = 0'y
	; C == 0'N -> skip_line(user), Answer = 0'n
	; C == (0' ) -> get_code(user,C), get_answer(Answer)
	; skip_line(user), Answer = C
	).

