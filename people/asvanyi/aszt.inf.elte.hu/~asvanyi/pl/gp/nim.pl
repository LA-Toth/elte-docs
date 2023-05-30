% This program plays Nim. This is a game for two players.

%   There are N piles of matches, and the players take turns to remove 
% some of the matches (up to all) in a pile. The winner is the player,
% who takes the last match. I give a common starting position, with four piles
% of 1, 3, 5 and 7 matches respectively.

:- module(nim,[nim/0,game/0]).

:- dynamic init/2.  % This allows You to redefine the starting position.

nim :-      % The main program.
    repeat,
	     nl,
	     write('Type in the list of piles, please.'), nl,
		  write('Mi = The number of matches in the i-th pile.'), nl,
		  write('[M1,M2,...,Mn]. = '), flush_output(user),
		  read(Piles), skip_line,
		  pile_list(Piles), 
	 !,
	 retractall(init(_,_)),
	 nl,
	 yesno('Do You want to start play?', 
		    assert(init(you,Piles)),
		    assert(init(computer,Piles))),
	 game.  

pile_list([X|Xs]) :- !, integer(X), X>0, pile_list(Xs).
pile_list([]).


% General framework

game :- 
    init(Player,Position),
    nl, show_pos(Position),
    play(Player,Position).

play(Player,Position) :-
    ( game_over(Position,Player,Result) -> announce(Result)
    ; choose_move(Player,Position,Move),
      move(Move,Position,Position1),
      show_pos(Position1),
      next_player(Player,Player1),
      !,
      play(Player1,Position1)
    ).


% Problem-specific predicates

init(you,[1,3,5,7]).

show_pos(Position) :- 
    write('The actual state of Nim: '), write(Position), nl.

game_over([],computer,'You').
game_over([],you,'I').

announce(Result) :- nl, write(Result), write(' won.'), nl.

next_player(you,computer).
next_player(computer,you).


% Making a determined move

move((PileIndex,Matches),Position,Position1) :-
    remove_from(PileIndex,Matches,Position,Position1).

remove_from(1,M,[M|Piles],Piles) :- !.
remove_from(1,M,[Pile|Piles],[Pile1|Piles]) :- M<Pile, !, Pile1 is Pile-M.
remove_from(I,M,[Pile|Piles],[Pile|Piles1]) :- I>1, % !,
                                               I1 is I-1,
					       remove_from(I1,M,Piles,Piles1).


% Choosing a move

choose_move(you,Position,(PileIndex,Matches)) :- 
  repeat,
    nl, write('Your turn:'), nl,
    write('Pile_Index , Number_of_Matches_to_be_Removed. = '), flush_output,
    ( read((PileIndex,Matches)), skip_line,
      integer(PileIndex), integer(Matches), Matches>0,
      possible_move(PileIndex,Matches,Position) -> !
    ; write('Impossible move.'),nl, fail
    ).
choose_move(computer,Position,(PileIndex,Matches)) :-
    nl, write('My turn:'), nl,    
    write('Pile_Index , Number_of_Matches_to_be_Removed. = '), flush_output,
    choose_computer_move(Position,PileIndex,Matches),
    write((PileIndex,Matches)), write('.'), nl.

choose_computer_move(Position,PileIndex,Matches) :-
    bin_sum(Position,0,S), S>0, !,
    calc_move(Position,S,1,PileIndex,Matches).
choose_computer_move(_,1,1) /* :- bin_sum(Position,0,0), ! */ .

possible_move(1,M,[M|_]) :- !.
possible_move(1,M,[Pile|_]) :- M<Pile, !.
possible_move(I,M,[_|Piles]) :- I>1, !, I1 is I-1,
				possible_move(I1,M,Piles).

bin_sum([Pile|Piles],S0,S1) :- STemp is S0\Pile, bin_sum(Piles,STemp,S1).
bin_sum([],S,S).

calc_move([Pile|Piles],S,PileIndex0,PileIndex,Matches) :-
    ( Pile /\ 1<<msb(S) =\= 0 ->
        Matches is Pile-(Pile\S), PileIndex = PileIndex0
    ; PileIndex1 is PileIndex0+1,
      calc_move(Piles,S,PileIndex1,PileIndex,Matches)
    ).

% yesno( Question, YesProgram, NoProgram ) :-
%     It types Question using write(user,Question),
%              and ' [yes] ' into the current line. 
%     If the user replies an empty line, or a line starting
%     with 'y' or 'Y', it performs 'YesProgram'.
%     If the user replies a line starting with 'n' or 'N',
%     it performs 'NoProgram'. It omits initial blanks of the input line.
yesno( Question, YesProgram, NoProgram ) :-
	write(user,Question), write(user,' [yes] '), flush_output(user),
	get_answer(Answer),
	( Answer == yes -> YesProgram
	; Answer == no -> NoProgram
	; write(user,'Wrong answer.\n'),
	    yesno( Question, YesProgram, NoProgram )
	).
 
% get_answer(Answer) :- Read a line from the standard input.
% If the line is empty, or its first nonblank character is y or Y, Answer=yes.
% If the first nonblank character is n or N, Answer=no.
% Otherwise Answer = the first nonblank character of this line.
get_answer(Answer) :-
	peek_code(user,C),
	( C == 0'\n -> skip_line(user), Answer = yes    % eoln
	; C == 0'Y -> skip_line(user), Answer = yes
	; C == 0'y -> skip_line(user), Answer = yes
	; C == 0'N -> skip_line(user), Answer = no
	; C == 0'n -> skip_line(user), Answer = no
	; C == (0' ) -> get_code(user,C), get_answer(Answer)
	; skip_line(user), atom_code(A,C), Answer = A
	).

% Todo: safe_read/1.
