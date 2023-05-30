:- module( bw, [ init_bw/1, bw/0, arc/3, goal/1, h/2 ] ).

/* This program solves the 'Black and White Problem'.

Goal: `11...1 22...2'

    `1' represents black, `2' represents white.
    (The position of the blank (represented by zero) may vary.)

Initial state: Any permutation of 'Goal'.

Steps:
    - Slip a coin (black or white).
    - `black' may jump over `white' to the left.
    - `white' may jump over `black' to the right.

This program needs graph_search/2.

It provides arc/3, goal/1, and  h/2 for graph_search/2.
*/

:- use_module(library(lists),[reverse/2]).

init_bw( Module ) :-
    use_module( Module, [ graph_search/2 ] ).

%%% The main program.
bw :- ask_for_coins(StartNode),
      graph_search(StartNode,Solution),
      show_sol(Solution),
      yesno( ['\nNew game?'], bw, write(user,'\n--bye\n') ).


%%%%%%%%%%%%%%%%%%%%%%% The state space graph %%%%%%%%%%%%%%%%%%%%%%%

% goal([1,1,...,1,0,2,2,...,2]).
% The position of '0' may vary.

goal(Node) :- \+ ( following_2(Node,L), member(1,L) ).

following_2([X|Xs],Ys) :-
    ( X==2 -> Ys=Xs
    ; following_2(Xs,Ys)
    ).

%%% Arcs in the state space graph:

% Jumping.
arc([0,2,1|L],[1,2,0|L],2).
arc([2,1,0|L],[0,1,2|L],2).

% Slipping.
arc([X,0|L],[0,X|L],1).
arc([0,X|L],[X,0|L],1).

% Jumping or slipping realized in the tail of the list.
arc([H|T1],[H|T2],C) :- H\==0, arc(T1,T2,C).

%%%%%%%%%%%%%%% Calculating the heuristic funtion. %%%%%%%%%%%%%%%

h(Node,H) :- inv(Node,0,Inv), H is 3*Inv.

inv([C|Cs],Inv0,Inv2) :-
    ( C==2 -> count1(Cs,Inv0,Inv1), inv(Cs,Inv1,Inv2)
    ; inv(Cs,Inv0,Inv2)
    ).
inv([],Inv,Inv).

count1([C|Cs],N0,N2) :-
    ( C==1 -> N1 is N0+1, count1(Cs,N1,N2)
    ; count1(Cs,N0,N2)
    ).
count1([],N,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Input %%%%%%%%%%%%%%%%%%%%%%%%%%

ask_for_coins(StartNode) :-
    repeat,
        assert('BLANK_NEEDED'),
        write( user, '\nType in the initial state, please!\n' ),
	write( user, '( black = 1, white = 2, blank = space_bar )\n' ),
        get_node(StartNode),
    !,
    nl(user).

get_node(Node) :-
    get_code(user,C),
    ( (C==0'1;C==0'2) -> X is C-0'0, Node=[X|L], get_node(L)
    ; C==(0' ), 'BLANK_NEEDED' -> 
        retractall('BLANK_NEEDED'), Node=[0|L], get_node(L)
    ; C==10, \+ 'BLANK_NEEDED' -> Node=[]
    ; % Error cases:
      ( C==(0' ) ->
	  write(user,'More than one blank in the initial state.\n'),
	  skip_line(user)
      ; C==10 -> write(user,'No blank in the initial state.\n')
      ; write(user,'Incorrect sign in the initial state.\n'),
	skip_line(user) 
      ),
      fail
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_sol(n2(G,[Goal|Path])) :-
    reverse([Goal|Path],[Start|Solution]),
    length(Solution,Steps),
    type_seq( [ 'The solution needs ', Steps,
                ' jumping/slipping.\nThe cost of it: ', G, '\n\n' ] ),
    yesno( ['Do You want to see the solution step by step?'],
           ( write( user, '\nStart Node:\n' ),
             show_steps([Start|Solution],0)
	   ),
           write( user, '\n OK\n\n' )
         ).

show_steps([X,Y|L],I) :-
    write_step(X),    
    I1 is I+1, number_codes(I1,I1L), length(I1L,LI1), tab(user,14-LI1),
    yesno( [I1,'. step?'],
           show_steps([Y|L],I1),
           write( user, '\n OK\n\n' )
         ).
show_steps([X],I1) :-
    write_step(X), number_codes(I1,I1L), length(I1L,LI1),
    Tab is 14-LI1, tab(user,Tab),
    write( user, 'This is the Goal Node.\n\n' ).

tab(S,N) :- ( N>0 -> put_code(S,0' ), N1 is N-1, tab(S,N1) ; true).

write_step(Node) :-
    \+ ( member(X,Node), \+ put_coin(X) ).

put_coin(C) :- C \=0, !,
    put_code(user,0' ), Code is 0'0+C, put_code(user,Code).
put_coin(0) :- put_code(user,0' ), put_code(user,0'_).

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
