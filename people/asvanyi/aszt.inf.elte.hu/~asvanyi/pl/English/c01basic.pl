%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                Chapter 1.: A biblical family database                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Facts: (Relations between entities and properties of entities.)

:- discontiguous father/2, mother/2, male/1, female/1.

	father(terach,'Abraham').		male(terach).
	father(terach,nachor).			male('Abraham').
	father(terach,haran).			male(nachor).
        father('Abraham','Ismael').             male('Ismael').
	father('Abraham','Isaac').		male(haran).
        father('Abraham','Unknown').
	father(haran,lot).			male('Isaac').
	father(haran,milcah).			male(lot).
	father(haran,yiscah).                   
        father('Isaac','Jacob').                male('Jacob').
        father('Jacob',joseph).                 male(joseph).
        father('Jacob',benjamin).               male(benjamin).

						female('Sarah').
	mother('Sarah','Isaac').		female(milcah).
	mother('Hagar','Ismael').		female(yiscah).
                                                female('Hagar').
                                                female('Unknown').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rules: (Defining new relations built on the top of facts.)
% (The variables of the rules are quantified universally.)

parent(X,Y) :- mother(X,Y).    % The union of two
parent(X,Y) :- father(X,Y).    % known relations.


% The intersection of relations and properties:

son(X,Y) :- parent(Y,X), male(X). 

son_(X,Y) :-
    male(X),
    parent(Y,X).

daughter(X,Y) :-
    parent(Y,X),
    female(X).

daughter_(X,Y) :-
    female(X),
    parent(Y,X).


% New variable in the body:

grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

grandfather(X,Y) :- father(X,Z), parent(Z,Y).

grandmother(X,Y) :- mother(X,Z), parent(Z,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recursive predicates:

% Finite search tree for the goal `?- ancestor(X,Y).' : (correct solution)
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y). % Recursive rule

% Infinite search tree:
% (It finds the solutions and then it goes into infinite loop,
%  because it finds the infinite branch of the search tree last.)
ancest(X,Y) :- parent(X,Y).
ancest(X,Y) :- ancest(Z,Y), parent(X,Z). % Recursive rule

% | ?- ancest('Hagar',X).
%
% X = 'Ismael' ? ;
%
% (Infinite loop...) <ctrl>C
%
% Prolog interruption (h for help)? a
% {Execution aborted}

% The same infinite search tree:
% (But it goes into infinite loop immediately,
%  because it finds the infinite branch of the search tree first.)
ances(X,Y) :- ances(Z,Y), parent(X,Z). % Infinite loop
ances(X,Y) :- parent(X,Y).

% | ?- ances('Hagar',X).
%
% (Infinite loop...) <ctrl>C
%
% Prolog interruption (h for help)? a
% {Execution aborted}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A short session:
% ----------------

% bash$ s3
% SICStus 3  #5: Fri Nov 1 15:49:55 MET 1996
% | ?- compile(c01basic).
% {compiling /teacher/asvanyi/pl/art/c01basic.pl...}
% {/teacher/asvanyi/pl/art/c01basic.pl compiled, 210 msec 10976 bytes}
% 
% yes
% | ?- ancestor(X,'Isaac').
%
% X = 'Sarah' ? ;
%
% X = 'Abraham' ? ;
%
% X = terach ? ;
%
% no
% | ?- ancest(X,'Isaac').
% 
% X = 'Sarah' ? ;
% 
% X = 'Abraham' ? ;
% 
% X = terach ? 
% 
% yes
% | ?- ancest(X,'Isaac').
%
% X = 'Sarah' ? ;
%
% X = 'Abraham' ? ;
%
% X = terach ? ;
%
% (Infinite loop...) <ctrl>C
%
% Prolog interruption (h for help)? a
% {Execution aborted}
% | ?- ances(X,'Isaac').
%
% (Infinite loop...) <ctrl>C
%
% Prolog interruption (h for help)? a
% {Execution aborted}
% | ?- halt.
% bash$ 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% universal fact:
% (The variables of the facts are quantified universally.)
eq(X,X). 
