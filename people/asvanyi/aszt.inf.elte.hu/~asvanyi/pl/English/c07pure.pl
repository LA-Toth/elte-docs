%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 How to write pure Prolog programs?                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 	parent(terach,abraham).		parent(abraham,isaac).
	parent(isaac,jacob).		parent(jacob,benjamin).

	ancestor(X,Y) :- parent(X,Y).
	ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).

%	ancestor(X,Y) :- parent(X,Y).
%	ancestor(X,Z) :- ancestor(Y,Z), parent(X,Y). %infinite branch

%  Program 7.1    Yet another family example

% Rule order influences the traversal of the search tree.
% Termination is made sure by finite search tree.
% Changes in goal order changes the search tree. It may become infinite.

num(0).
num(s(X)) :- num(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   merge(Xs,Ys,Zs) :- 
	Zs is an ordered list of integers obtained from 
	merging the ordered lists of integers Xs and Ys.
*/
     merge([X|Xs],[Y|Ys],[X|Zs]) :-
	X < Y, merge(Xs,[Y|Ys],Zs).
     merge([X|Xs],[Y|Ys],[X,Y|Zs]) :-
	X =:= Y, merge(Xs,Ys,Zs).
     merge([X|Xs],[Y|Ys],[Y|Zs]) :-
	X > Y, merge([X|Xs],Ys,Zs).
     merge([],[X|Xs],[X|Xs]).      % instead of merge([],Xs,Xs).
     merge(Xs,[],Xs).

%  Program 7.2    Merging ordered lists

% Avoid redundant solutions.

     min(X,Y,X) :- X=<Y.
     min(X,Y,Y) :- X>Y.     % instead of X>=Y

% Only one rule should cover a solution.
% ?- min(2,2,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode min_(+,+,-).     % with the red cut bellow

min_(X,Y,X) :- X=<Y, !.  % red cut! % ?- min(2,3,3). !!!
min_(_,Y,Y).             % with % min_(X,Y,Y) :- X>Y. % green cut above

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   member_check(X,Xs) :- X is a member of the list Xs.
*/
     member_check(X,[Y|_]) :- X == Y.
     member_check(X,[Y|Ys]) :- X \== Y, member_check(X,Ys).

%  Program 7.3    Checking for list membership

% Compare to: 

     member_(X,[X|_]).
     member_(X,[_|Ys]) :- member_(X,Ys).

% Compare to: 

     member_1(X,[Y|_]) :- X = Y.
     member_1(X,[Y|Ys]) :- X \= Y, member_1(X,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   select_first(X,Xs,Ys) :- 
	Ys is the list obtained by removing the 
	first occurrence of X from the list Xs.
*/
     select_first(X,[X|Xs],Xs).
     select_first(X,[Y|Ys],[Y|Zs]) :- X \= Y, select_first(X,Ys,Zs).

%  Program 7.4    Selecting the first occurrence of an element from a list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   non_member(X,Xs) :- X is not a member of the list Xs.
*/
     non_member(X,[Y|Ys]) :- X \== Y, non_member(X,Ys).
     non_member(_,[]).

%  Program 7.5    Nonmembership of a list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   members(Xs,Ys) :- Each element of the list Xs is an element of the list Ys.
*/
     members([X|Xs],Ys) :- member(X,Ys), members(Xs,Ys).
     members([],_).

%  Program 7.6    Testing for a subset

:- use_module(library(lists),[select/3]).

% Both arguments must be complete lists for termination.
% The elements in the subsets may be duplicated.
% The elements in the subsets may be given in different orders.

% member(X,[X|_]).    % member/2 is imported from library(lists)
% member(X,[_|Ys]) :- member(X,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   selects(Xs,Ys) :- The list Xs is a sub-bag of the list Ys.
*/
     selects([X|Xs],Ys) :- select(X,Ys,Ys1), selects(Xs,Ys1).
     selects([],_).

%	select(X,[X|Xs],Xs).       % select/3 is imported from library(lists)
%	select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

%  Program 7.7    Testing for a sub-bag

% The second argument must be a complete list for termination.
% The elements in the subsets will not be duplicated.
% The elements in the sub-bags have =< multiplicity.
% The elements in the subsets (sub-bags) are accepted in different orders.
% We get the sub-bags in the possible orders.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The elements are accepted/received only in the order corresponding to that
% in the second list, if we write the program:

sublist([],_).
sublist([X|Xs],L) :- append(_,[X|Ys],L), sublist(Xs,Ys).

% append([], List, List).
% append([Head|Tail], List, [Head|Rest]) :- 
% 	append(Tail, List, Rest).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following program is only for testing (but not for generating) sub-bags:

  selects_([X|Xs],Ys) :- select_(X,Ys,Ys1), selects_(Xs,Ys1).
  selects_([],_).

  select_(X,[X|Xs],Xs).
  select_(X,[Y|Ys],[Y|Zs]) :- X\=Y, select_(X,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
     translate(Words,Mots) :- 
	Mots is a list of French words that is the 
	translation of the list of English words Words.
*/
     translate([Word|Words],[Mot|Mots]) :- 
        dict(Word,Mot), translate(Words,Mots).
     translate([],[]).

     dict(the,le).                 dict(dog,chien).         
     dict(chases,chasse).          dict(cat,chat).

%  Program 7.8   Translating word for word

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
	no_doubles(Xs,Ys) :-
		Ys is the list obtained by removing 
		duplicate elements from the list Xs.
*/

no_doubles([X|Xs],Ys) :-
	member_check(X,Xs), no_doubles(Xs,Ys).
no_doubles([X|Xs],[X|Ys]) :-
	non_member(X,Xs), no_doubles(Xs,Ys).
no_doubles([],[]).

%     non_member(X,[Y|Ys]) :- X \== Y, non_member(X,Ys).
%     non_member(X,[]).

%  Program 7.9   Removing duplicates from a list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
     nd_reverse(Xs,Ys) :- 
	Ys is the reversal of the list obtained by 
	removing duplicate elements from the list Xs.
*/
     nd_reverse(Xs,Ys) :- nd_reverse(Xs,[],Ys).

     nd_reverse([X|Xs],Revs,Ys) :-
	member_check(X,Revs), nd_reverse(Xs,Revs,Ys).
     nd_reverse([X|Xs],Revs,Ys) :-
	non_member(X,Revs), nd_reverse(Xs,[X|Revs],Ys).
     nd_reverse([],Ys,Ys).

%     non_member(X,[Y|Ys]) :- X \== Y, non_member(X,Ys).
%     non_member(_,[]).

%  Program 7.10   Reversing with no duplicates

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
