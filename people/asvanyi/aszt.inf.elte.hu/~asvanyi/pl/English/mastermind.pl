:- module( mastermind, [ master/0, mastermind/0 ] ). %% 7295

:- use_module( library(lists), [ nth1/3 ] ).

:- dynamic history/2.

%% repeat.              % repeat is built-in
%% repeat :- repeat.

master :-
    welcome,
    repeat,
        mastermind,
        yesno('\nDo you want a new turn?',fail,true),
    !.

welcome :-   
    type_seq(['\n               This is Mastermind.\n\n',
	      '  Find out four different digits, please!\n',
	      '  Press <RET>, when You are ready.\n\n']),
    skip_line(user).

mastermind :-
    retractall(history(_,_)),
    ( generate(Tip),		% Generate and ...
      consistent(Tip),		% test 1
      ask(Tip,Score),
      assertz(history(Tip,Score)),
      exact_tip(Score)		% test 2
    ->
      type_history
    ;
      write(user,'\nYour answers are inconsistent.\n'),
      type_history
    ).

generate([A,B,C,D]) :-
    digit(A,0), digit(B,0), B\==A, digit(C,0), C\==A, C\==B,
    digit(D,0), D\==A, D\==B, D\==C.

digit(B,B).
digit(D,B) :- B<9, B1 is B+1, digit(D,B1).

consistent(NewTip) :-
    \+ ( history(OldTip,Score), \+ match(OldTip,NewTip,Score,1) ).

%% for_each(A,B) :- \+ ( A, \+B ).

match([X|Xs],Tip,B-C,I) :-
    I1 is I+1,
    ( nth1(J,Tip,X) ->
        ( J==I -> B1 is B-1, match(Xs,Tip,B1-C,I1)
        ; C1 is C-1, match(Xs,Tip,B-C1,I1) 
        )
    ; match(Xs,Tip,B-C,I1) 
    ).
match([],_,0-0,_).

ask(Tip,Bull-Cow) :-
    type_seq(['\nNew tip: ',Tip,
	      '\nScore? (Number of bulls and cows)\n']), 
    repeat,
        write(user,'Bulls Cows = '), flush_output(user),
	get(user,B), get(user,C), skip_line(user),
        Bull is B-0'0,  Cow is C-0'0,
        Bull>=0, Cow>=0, Bull+Cow=<4,
    !.

exact_tip(4-0).

type_history :-
    write(user,'\nHistory:\n   Tip        Score\n'),
    history(Tip,Score),
    type_seq([Tip,'     ',Score,'\n']), 
    fail.
type_history.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get(S,C) :- get the code of the next character
%             which is greater than the blank (space) character. 
get(S,C) :-
    get_code(S,D),
    ( D > (0' ) -> C = D
    ; get(S,C)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%% get_answer(Answer) :- Read a line from the standard input.
%% If the line is empty, or its first nonblank character is y or Y, Answer=yes.
%% If the first nonblank character is n or N, Answer=no.
%% Otherwise Answer = the first nonblank character of this line.
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


type_seq([]).
type_seq([X|Xs]) :- write(user,X), type_seq(Xs).

