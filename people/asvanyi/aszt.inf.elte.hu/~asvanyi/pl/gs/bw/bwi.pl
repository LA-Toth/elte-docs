:- module( bwi, [ init_bw/1, bw/0, arc/3, goal/1, h/2 ] ).

/* This program solves the 'Black and White Problem'.

Goal: 'bb...b_ww...w'

    'b' represents black, 'w' represents white.
    The position of the blank represented by '_' may vary.)

Initial state: Any permutation of 'Goal'.

Steps:
    - Slip a coin (black or white).
    - 'black' may jump over 'white' to the left.
    - 'white' may jump over 'black' to the right.

This program needs graph_search/2.

It provides arc/3, goal/1, and  h/2 for graph_search/2.
*/

:- use_module(library(lists),[reverse/2]).

init_bw( Module ) :-
    use_module( Module, [ graph_search/2 ] ).


:- dynamic state_length/1, whites/1, coins_in_goal/1.

%%% The main program.
bw :- ask_for_coins(StartNode),
      ms(graph_search(StartNode,Solution),MS),
      show_sol(Solution,MS).


%%%%%%%%%%%%%%%%%%%%%% The state space graph %%%%%%%%%%%%%%%%%%%%%

% n2(S,B) is the representation of a node.
% S is the sequence of coins without the empty space.
% S is an integer, that is, a sequence of bits.
% bit '0' stands for 'b' (black coin),
% bit '1' stands for 'w' (white coin).
% Predicate 'state_length(N)' stands
%   for the length of the sequence of bits.
% The position of the empty state is represented by B in n2(S,B).
% B counts the number of coins right to the empty space.

goal(n2(S,_)) :- coins_in_goal(S). 

%%% Arcs in the state space graph:

% A black (0) jumps to the left.
arc(n2(S,B),n2(S2,B2),2) :-
    B>1, WhiteCoin is 1<<(B-1), 
    WhiteCoin/\S =\= 0, WhiteCoin>>1/\S =:= 0, 
    % -> 
    S2 is S/\(\(WhiteCoin))\/WhiteCoin>>1, B2 is B-2.

% A white (1) jumps to the right.
arc(n2(S,B),n2(S2,B2),2) :-
    state_length(N), B<N-1, WhiteCoin is 1<<(B+1),
    WhiteCoin/\S =\= 0, WhiteCoin>>1/\S =:= 0,
    % ->
    S2 is S/\(\(WhiteCoin))\/WhiteCoin>>1, B2 is B+2.

% Slip to the left.
arc(n2(S,B),n2(S,B1),1) :-
    B>0, % ->
    B1 is B-1.

% Slip to the right.
arc(n2(S,B),n2(S,B1),1) :-
    state_length(N), B<N, % ->
    B1 is B+1.

%%%%%%%%%%%%%%% Calculating the heuristic funtion. %%%%%%%%%%%%%%%

h(n2(State,_),H) :- whites(W), inv(W,State,0,Inv), H is 3*Inv.

inv(0,_,Inv0,Inv) :-
    !, Inv=Inv0.
inv(W0,State0,Inv0,Inv) :- 
    State0/\1=:=1, !, 
    W1 is W0-1, State1 is State0>>1, 
    inv(W1,State1,Inv0,Inv).
inv(W0,State0,Inv0,Inv) :- 
    % State0/\1=:=0, !,
    State1 is State0>>1, Inv1 is Inv0+W0, 
    inv(W0,State1,Inv1,Inv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_for_coins(StartNode) :-
    repeat,
        write(user,'\nType in the initial state, please!\n'), 
        write(user,'( black = b, white = w, blank = _ )\n'),
        get_node(StartNode),
    !,
    nl.

get_node(n2(State,EmptyPos)) :- 
    retractall(state_length(_)), retractall(whites(_)),
    retractall(coins_in_goal(_)),
    get_code(user,C),
    get_node(C,0,0,0,State,EmptyPosLeft,N,W),
    EmptyPos is N-EmptyPosLeft,
    CoinsInGoal is 1<<W-1,
    asserta(state_length(N)), asserta(whites(W)),
    asserta(coins_in_goal(CoinsInGoal)).

get_node(0'\n,State,N,W,State,EmptyPos,N,W) :-
    nonvar(EmptyPos), !.
get_node(0'\n,_,_,_,_,EmptyPos,_,_) :-
    var(EmptyPos),
    !,
    write('No empty position (_) in the initial state.\n'),
    fail.
get_node(0'_,State0,Index0,W0,State,EmptyPos,N,W) :-
    var(EmptyPos), !,
    EmptyPos = Index0, get_code(user,C),
    get_node(C,State0,Index0,W0,State,EmptyPos,N,W).
get_node(0'_,_,_,_,_,EmptyPos,_,_) :-
    nonvar(EmptyPos),
    !,
    write(user,
      'More than one empty position (_) in the initial state.\n'),
    skip_line(user),
    fail.
get_node(0'b,State0,Index0,W0,State,EmptyPos,N,W) :- !,
    State1 is State0<<1, Index1 is Index0+1, get_code(user,C),
    get_node(C,State1,Index1,W0,State,EmptyPos,N,W).
get_node(0'w,State0,Index0,W0,State,EmptyPos,N,W) :- !,
    State1 is State0<<1\/1, Index1 is Index0+1, W1 is W0+1,
    get_code(user,C),
    get_node(C,State1,Index1,W1,State,EmptyPos,N,W).
get_node(C,_,_,_,_,_,_,_) :-
    nonmember(C,"\n_bw"),
    write(user,'Incorrect sign in the initial state.\n'),
    skip_line(user),
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_sol(n2(G,Path),MS) :-
    reverse(Path,Solution),
    length(Solution,Steps1), Steps is Steps1-1,
    Jumps is G-Steps, Slips is Steps-Jumps,
    type_seq(['The solution needs ',Steps,' moves ( ',
	      Jumps, ' jumps, ', Slips, ' slips ),\n',
               'and ',MS,' milliseconds to be found.\n',
	      'The cost of it is ', G, '.\n\n' ]),
    yesno( ['Do You want to see the solution step by step?'],
	   ( write( user, '\nStart Node:\n' ),
	     show_steps(Solution,0)
	   ),
	   write( user, '\n OK\n\n' )
	 ).

show_steps([X,Y|L],I) :- !,
    write_step(X),    
    I1 is I+1, number_codes(I1,I1L), length(I1L,LI1),
    Tab is 14-LI1, tab(user,Tab),
    yesno( [I1,'. step?'],
           show_steps([Y|L],I1),
           write( user, '\n OK\n\n' )
         ).
show_steps([X],I1) :-
    write_step(X), number_codes(I1,I1L), length(I1L,LI1),
    Tab is 14-LI1, tab(user,Tab),
    write( user, 'This is the Goal Node.\n\n' ).

write_step(n2(State,EmptyPos)) :-
    state_length(N),
    State0 is State>>EmptyPos, N0 is N-EmptyPos,
    State1 is State mod (1<<EmptyPos),
    write01(N0,State0), write(user,'_ '), write01(EmptyPos,State1).

write01(0,_) :- !.
write01(N,State) :-
    N1 is N-1, C is(1<<N1/\State)>>N1,
    ( C==0 -> write(user,'b ')
    ; write(user,'w ')
    ),
    write01(N1,State). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% yesno( Question, YesProgram, NoProgram ) :-
%     It types Question using type_seq(Question),
%              and ' [yes] ' into the current line. 
%     If the user replies an empty line, or a line starting
%     with 'y' or 'Y', it performs 'YesProgram'.
%     If the user replies a line starting with 'n' or 'N',
%     it performs 'NoProgram'. It omits initial blanks of the input line.
yesno( Question, YesProgram, NoProgram ) :-
	type_seq(Question), write(user,' [yes] '), flush_output(user),
	get_answer(Answer),
	( Answer == yes -> YesProgram
	; Answer == no -> NoProgram
	; write(user,'Wrong answer.\n'),
	  yesno( Question, YesProgram, NoProgram )
	).

% type_seq(Xs) :- write the elements of a proper list onto the standard output.
type_seq([]).
type_seq([X|Xs]) :- write(user,X), type_seq(Xs).

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

% ms(Goal,MilliSeconds) :-
%     The Prolog goal Goal needs Milliseconds milliseconds to run.
ms(Goal,MilliSeconds) :-
    statistics(runtime,_), Goal, statistics(runtime,[_,MilliSeconds]).

tab(S,N) :- ( N>0 -> put_code(S,0' ), N1 is N-1, tab(S,N1) ; true).
