%% -*- Mode: Prolog; coding: utf-8 -*- 

%%% :- module( m, [ father/2, mother/2, male/1, female/1, 
%%% 		likes/2, 'Abraham\'s daughter'/1,
%%% 		parent/2, son/2, daughter/2,
%%% 		grandparent/2, 'great-grandparent'/2,
%%% 		ancestor/2, ancestor0/2, ancestor00/2,
%%% 		sibling/2, sibling_/2, son_/2,
%%% 		list/1, member_/2, append_/3,
%%%             rev_app/3, reverse/2, intersection/3,
%%% 		safe_member/2, union/3, malfunctioning_union/3,
%%% 		(\+=)/2, does_not_have/2, nonmember_/2, 
%%% 		member1/2, member_check/2,
%%%             add/3, mult0/3, mult/3, 
%%% 		grandparent_/2, sorted_insert/3,
%%%             substitute/4, aA/2,
%%% 		connected/2, reach_from/2, path/3,
%%% 		appf/2, ground_/1, read1term/1,
%%% 		forall/2, collect/3, comp/1, comp/0,
%%% 		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 		brother_or_sister/2, aunt_or_uncle/2,
%%%             parent_in_law/2, 
%%% 		prefix/2, suffix/2,
%%%             sublist_a/2, sublist_b/2, sublist_c/2,
%%% 		subseq/2, divide/3, sorted_union/3,
%%% 		inorder/2, inorder_opt/2, 
%%% 		empty_tree/1, tree_ins/3, tree_has/2, tree_del/3,
%%% 		insertsort/2, mergesort/2, unionsort/2,
%%%             find_all/3,
%%% 		breadth_first_search/3 ] ).
 
:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(100)]).

father('Abraham','Isaac').        father('Abraham','Ishmael').
father('Abraham','Anon').
father('Isaac','Jacob').          father('Isaac','Esau').

mother('Sarah','Isaac').          mother('Hagar','Ishmael').
mother('Rebeka','Jacob').        mother('Rebeka','Esau').

male('Abraham').    male('Isaac').    male('Ishmael').
male('Jacob').      male('Esau').

female('Sarah').         female('Hagar').       female('Rebeka').
female('Anon').

'Abraham\'s daughter'(X) :- father('Abraham',X), female(X).

likes(_anybody,'Sarah').

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).

son(X,Y) :- parent(Y,X), male(X).

daughter(X,Y) :- parent(Y,X), female(X).

grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

%% ancestor(X,Y) :- X is ancestor of Y.

'great-grandparent'(X,Y) :- parent(X,Z), grandparent(Z,Y).

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% These goals have infinite search trees:
%% | ?- ancestor0(X,Y).
%% | ?- ancestor00(X,Y).

ancestor0(X,Y) :- parent0(X,Y).
ancestor0(X,Y) :- parent0(X,Z), ancestor0(Z,Y).

parent0('God','God').

ancestor00(X,Y) :- parent(X,Y).
ancestor00(X,Y) :- ancestor00(X,Z), ancestor00(Z,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

father(SomeBody) :- father(SomeBody,_Child).

father_and_son(SomeBody) :- 
    father(SomeBody,_), parent(_,SomeBody).
%%% _ is the anonym logic variable.
%%% All the occurences of _ are different.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
| ?- 1.5 == 0.15e1.
yes

%% List notation

%% Empty list: []

%% Nonepty lists:
| ?- .(First,Remainder) == [First|Remainder].
yes
| ?- .(X,Xs) == [X|Xs].
%%% Xs, Ys, Zs, Items,
%%% ... A plural variable name suggests 
%%% that the variable denotes a (possibly unknown) list.
%%% (This notation comes from "The Art of Prolog".)
yes
| ?- .(A,[]) == [A|[]].
yes
| ?- .(A,[]) == [A].
yes
| ?- .(A,.(B,[])) == [A|[B|[]]].
yes
| ?- .(A,.(B,[])) == [A,B|[]].
yes
| ?- .(A,.(B,[])) == [A,B].
yes
| ?- .(A,.(B,.(C,[]))) == [A|[B|[C|[]]]].
yes
| ?- .(A,.(B,.(C,[]))) == [A,B|[C|[]]].
yes
| ?- .(A,.(B,.(C,[]))) == [A,B,C|[]].
yes
| ?- .(A,.(B,.(C,[]))) == [A,B,C].
yes
| ?- .(A,.(B,.(C,[]))) == [A,B|[C]].
yes
| ?- .(A,.(B,.(C,[]))) == [A|[B,C]].
yes
| ?- .(A,.(B,.(C,[]))) \== [[A]|[B,C]].
yes
| ?- .(.(A,[]),.(B,.(C,[]))) == [[A]|[B,C]].
yes
*/

list([]).
list([_X|Xs]) :- list(Xs).

%% PreCond: Xs is a proper list.
%% member_(X,Xs) :- X is a member of list Xs.
member_(X,[X|_Xs]).
member_(X,[_X|Xs]) :- member_(X,Xs).

%% PreCond: Xs or XsYs is a proper list,
%% the other two parameters are lists.
%% append_(Xs,Ys,XsYs) :-
%%    appending lists Xs and Ys we receive list XsYs.
append_([],Ys,Ys).
append_([X|Xs],Ys,[X|Zs]) :- append_(Xs,Ys,Zs).

%% PreCond: Xs is a proper list, Ys and Zs are lists.
%% rev_app(Xs,Ys,Zs) :- 
rev_app([],Ys,Ys).
rev_app([X|Xs],Ys,Zs) :- rev_app(Xs,[X|Ys],Zs).

%% PreCond: Xs is a proper list.
%% reverse(Xs,Ys) :- the reverse of Xs is Ys.
reverse(Xs,Ys) :- rev_app(Xs,[],Ys).

%% LI = N+2 in Theta(N)

%% Naive reverse.
%% LI: Theta(N*N) where N is the length of the list (N=|Xs|)
%% r(Xs,Ys) :- the reverse of Xs is Ys.
r([],[]).
r([X|Xs],Zs) :-
    r(Xs,Ys),
    append_(Ys,[X],Zs).

%% LI = N+N-1+N-2+...+1 + N+1 = (N+1)*(N+2)/2 in Theta(N^2)

%% safe_member(X,Xs) :- X is member of list Xs (NSTO)
safe_member(X,[Z|_Xs]) :- unify_with_occurs_check(X,Z).
safe_member(X,[_X|Xs]) :- safe_member(X,Xs).

%%% member_(X,[X|_Xs]).
%%% member_(X,[_X|Xs]) :- member_(X,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ( If -> Then ; Else )
%% and local cut : ->

%% PreCond: Xs and Ys are ground lists.
%% union(Xs,Ys,Zs) :-
%%      Those members of Xs which are not members of Ys 
%%      concatenated in order before Ys produces list Zs.
union([],Ys,Ys).
union([X|Xs],Ys,Zs) :-
    ( member_(X,Ys) -> union(Xs,Ys,Zs)
    %%! OK: Local cut is immediately after the condition.
    ; Zs = [X|Us], union(Xs,Ys,Us)
    ).

%% X=X.

malfunctioning_union([],Ys,Ys).
malfunctioning_union([X|Xs],Ys,Zs) :-
    ( member_(X,Ys), malfunctioning_union(Xs,Ys,Zs)
    %%! Local cut is missing.
    ; Zs = [X|Us], malfunctioning_union(Xs,Ys,Us)
    ).

malfunctioning_union_2([],Ys,Ys).
malfunctioning_union_2([X|Xs],Ys,Zs) :-
    ( member_(X,Ys), malfunctioning_union_2(Xs,Ys,Zs) -> true
    %%! Local cut comes too late.
    ; Zs = [X|Us], malfunctioning_union_2(Xs,Ys,Us)
    ).

%%% !!! CUT AS EARLY AS POSSIBLE !!! %%%

%% \+=(X,Y) :- X does not match Y.
\+=(X,Y) :- ( X=Y -> fail ; true ).

%% PreCond: Xs is a proper list.
%% does_not_have(Xs,Y) :- no member of Xs matches Y.
does_not_have([],_Y).
does_not_have([X|Xs],Y) :-
    ( X = Y -> fail
    ; does_not_have(Xs,Y)
    ).

%% nonmember_(X,Xs) :- does_not_have(Xs,X).
nonmember_(X,Xs) :- ( member_(X,Xs) -> fail ; true ).

%% PreCond: Xs and Ys are gound lists.
%% intersection(Xs,Ys,Zs) :- 
%%     those members of Xs which are found on Ys, too,  
%%     form in order proper list Zs.
intersection([],_Ys,[]).
intersection([X|Xs],Ys,Zs) :-
    ( member_(X,Ys) ->
          Zs = [X|Ms], intersection(Xs,Ys,Ms)
    ; intersection(Xs,Ys,Zs)
    ).

member1(X,Xs) :- ( member_(X,Xs) -> true ).

%% ( If -> Then ) is equivalent to ( If -> Then ; fail )

%% member_check(_X,[]) :- fail.
member_check(X,[Y|Ys]) :-
    ( X = Y -> true
    ; member_check(X,Ys)
    ).

%% Common cut

min0(M,N,M) :- M =< N.
min0(M,N,N) :- M > N.

%% Green cut

min1(M,N,M) :- M =< N, !. 
min1(M,N,N) :- M > N.

%% Red cut

min2(M,N,M) :- M =< N, !. %%! wrong
min2(_M,N,N).

min3(M,N,M) :- M =< N. %%! even worse
min3(_M,N,N).

%% Safe use of red cut:
min4(M,N,Min) :- M =< N, !, Min = M.
%%! Output matching is after the cut. Cut as early as possible.
%%!  (This rule is used correctly in the previous rule.)
min4(_M,N,N). 

%% The same with local cut:
min(M,N,Min) :-
    ( M =< N -> Min = M  %%! Cut as early as possible.
    ; Min = N            %%! (This rule is used correctly here) 

    ).

minWrong3(M,N,Min) :-
    ( M =< N, Min = M
    ; Min = N
    ).

minWrong4(M,N,Min) :-
    ( M =< N, Min = M -> true %%! Cut comes too late.
    ; Min = N
    ).

%% PreCond: Xs and Ys are ground lists.
%% union(Xs,Ys,Zs) :-
%%      Those members of Xs which are not members of Ys 
%%      concatenated in order before Ys produces list Zs.

union0([],Ys,Ys).
union0([X|Xs],Ys,Zs) :- member(X,Ys), union0(Xs,Ys,Zs).
%%! without cut (naive)
union0([X|Xs],Ys,[X|Zs]) :- \+member(X,Ys), union0(Xs,Ys,Zs).

union1([],Ys,Ys).
union1([X|Xs],Ys,Zs) :- member(X,Ys), !, union1(Xs,Ys,Zs).
%%! green cut
union1([X|Xs],Ys,[X|Zs]) :- \+member(X,Ys), union1(Xs,Ys,Zs).

union2([],Ys,Ys).
union2([X|Xs],Ys,Zs) :- member(X,Ys), !, union2(Xs,Ys,Zs).
%%! safe red cut: Output matching above is after the cut.
union2([X|Xs],Ys,[X|Zs]) :- union2(Xs,Ys,Zs).

%% PreCond: Xs and Ys are gound lists.
%% intersection(Xs,Ys,Zs) :- 
%%     those members of Xs which are found on Ys, too,  
%%     form in order proper list Zs.

intersection0([],_Ys,[]).
intersection0([X|Xs],Ys,[X|Is]) :- %%! without cut (naive)
    member(X,Ys), intersection0(Xs,Ys,Is).
intersection0([X|Xs],Ys,Zs) :-
    \+member(X,Ys), intersection0(Xs,Ys,Zs).

intersection1([],_Ys,[]).
intersection1([X|Xs],Ys,[X|Is]) :- %%! with green cut
    member(X,Ys), !, intersection1(Xs,Ys,Is).
intersection1([X|Xs],Ys,Zs) :-
    \+member(X,Ys), intersection1(Xs,Ys,Zs).

intersection1unsafe([],_Ys,[]).
intersection1unsafe([X|Xs],Ys,[X|Is]) :- %%! unsafe red cut
    member(X,Ys), !, intersection1unsafe(Xs,Ys,Is).
intersection1unsafe([_X|Xs],Ys,Zs) :-
    intersection1unsafe(Xs,Ys,Zs).

/*
| ?- intersection1unsafe([1,2,4,7],[3,1,8,4],[4]).
%%! false result:		%
yes
| ?- intersection1unsafe([1,2,4,7],[3,1,8,4],[]).
%%! false result:		%
yes
| ?- intersection1unsafe([1,2,4,7],[3,1,8,4],[1]).
%%! false result		%
yes
| ?- intersection1unsafe([1,2,4,7],[3,1,8,4],[1,4]).
%%! correct result		%
yes
| ?- intersection1unsafe([1,2,4,7],[3,1,8,4],Is).
%%! correct result
Is = [1,4] ? ;
no
*/

intersection2([],_Ys,[]).
%%! with green cut, output matching comes after the cut:
intersection2([X|Xs],Ys,Zs) :-
    member(X,Ys), !, Zs=[X|Is], intersection2(Xs,Ys,Is).
intersection2([X|Xs],Ys,Zs) :-
    \+member(X,Ys), intersection2(Xs,Ys,Zs).

intersection3([],_Ys,[]).
%%! with safe red cut: output matching comes after the cut
intersection3([X|Xs],Ys,Zs) :-
    member(X,Ys), !, Zs=[X|Is], intersection3(Xs,Ys,Is).
intersection3([_X|Xs],Ys,Zs) :-
    intersection3(Xs,Ys,Zs).

%% PreCond: Xs and Ys are proper lists, sorted strictly increasingly.
%% sortedIntersection(Xs,Ys,Zs) :- Zs is the sorted intersection of Xs and Ys.

sortedIntersection([],_Ys,[]).
sortedIntersection([_X|_Xs],[],[]) :- !. %% green cuts
sortedIntersection([X|Xs],[Y|Ys],Zs) :-
    X@<Y, !, sortedIntersection(Xs,[Y|Ys],Zs).
sortedIntersection([X|Xs],[Y|Ys],Zs) :-
    X@>Y, !, sortedIntersection([X|Xs],Ys,Zs).
sortedIntersection([X|Xs],[Y|Ys],[X|Zs]) :-
    X==Y, sortedIntersection(Xs,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Negation

unmarried_Student(X) :- student(X), unmarried(X).

unmarried_Student_(X) :- unmarried(X), student(X). % wrong

unmarried(X) :- ( married(X) -> fail ; true ).

student('Peter'). student('John').
student('James').

married('Peter'). married('Joseph').

not(P) :- ( P -> fail ; true ).

unmarriedStudent(X) :- student(X), not(married(X)).

unmarriedStudent_(X) :- not(married(X)), student(X). % wrong

unmarried_student(X) :- student(X), \+ married(X).

unmarried_student_(X) :- \+ married(X), student(X). % wrong

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Arithmetics:
%% ------------

%% addv(V1,V2,V) :- Vector V is the sum of vectors V1 and V2. 
addv([],[],[]).
addv([X|Xs],[Y|Ys],[Z|Zs]) :- 
    Z is X+Y, addv(Xs,Ys,Zs).

%% X=X.

%% mult0(V,W,S) :- %% S is V1*W1+V2*W2+...+Vlast*Wlast
%%     S is the scalar product of vectors V and W.
mult0([],[],0).
mult0([X|Xs],[Y|Ys],S) :-
    mult0(Xs,Ys,S0), S is S0+X*Y.

mult(V1,V2,S) :- mult(V1,V2,0,S).

%% mult(V1,V2,A,S) :- S = A+V1*V2.
mult([],[],A,A).
mult([X|Xs],[Y|Ys],A0,S) :-
    A1 is A0+X*Y, mult(Xs,Ys,A1,S).

fac(N,F) :- integer(N), N>0, fac_(N,1,F).
fac(0,1).

fac_(N,A,F) :- %% F is N!*A
    ( N>1 -> AtimesN is A*N, Nminus1 is N-1,
             fac_(Nminus1,AtimesN,F)
    ; F = A
    ).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Types of terms:

%% var(Term)
       %% Up_32gHÉÁÓÜÖöü_óíÍúÚé, _like_in_C, _
%% nonvar(Term)
%%     atomic(Term)
%%         number(Term)
%%             float(Term)
                   %% (64 bits, corresponds to C double type)
%%             integer(Term)
                   %% [-(2^2147483616)..(2^2147483616-1)]
                   %% 0'a == 97, 0x73FA4, 0o7, 0b101
%%         atom(Term)
               %% ab_3c_d4, 'Any\' \n text!',
               %%  ~^$+-*.:/\<=>@&#÷°×¤, [], {}, !, ;
%%    compound(Term)
          %%  atom(Term1,...,TermN)
          %%  [X,Y,Z|Xs] == .(X,.(Y,.(Z,Xs))) [X,Y]==[X,Y|[]]

%% simple(Term) iff \+compound(Term) (in SICStus)

%% 2022

grandparent_(X,Y) :- 
    ( var(X), nonvar(Y) -> parent(Z,Y), parent(X,Z)
    ; parent(X,Z), parent(Z,Y)
    ).

%% Operators

:- op(700,xfx,=::=).		% directive

%% :- write(user,'It is after defining op(700,xfx,=::=).\n').

X=::=Y :-
    ( number(X), number(Y) -> X=:=Y
    ; X==Y
    ).

%%% :-(=::=(X,Y),;( ->(','(number(X),number(Y)),X=:=Y),X==Y)).

%% :- op( 1200, xfx, [ :-, --> ]).
%% :- op( 1200,  fx, [ :-, ?- ]).
%% :- op( 1100, xfy, [ ; ]). 
%% :- op( 1050, xfy, [ -> ]).
%% :- op( 1000, xfy, [ ',' ]).
%% :- op(  900,  fy,   \+ ).
%% :- op(  700, xfx, [ =, \=, ==, \==, @<, @>, @=<, @>=,
%%                     =.., is, =:=, =\=, <, >, =<, >= ]).
%% :- op(  500, yfx, [ +, -, /\, \/ ]).
%% :- op(  400, yfx, [ *, /, //, mod, rem, <<, >> ]).
%% :- op(  200, xfx,   ** ).    :- op( 200, xfy, ^ ).
%% :- op(  200,  fy, [ -, \ ]).


%% Standard order of Prolog terms 

%% Var @< Float @< Integer @< Atom @< Compound.

%% :- op(  700, xfx, [ ==, \==, @<, @>, @=<, @>= ]).
%% operators of standard comparisons

%% :- op(  700, xfx, [ =:=, =\=, <, >, =<, >= ]).
%% operators of arithmetic comparisons

%% PreCond: Ys is a proper list sorted nondecreasingly
%%          according to the standard order.
%% sorted_insert(Ys,X,Zs) :-
%%     Zs is received by the sorted insert of X into Ys.
sorted_insert([],X,[X]).
sorted_insert([Y|Ys],X,Zs) :- 
    ( X @=< Y -> Zs = [X,Y|Ys]
    ; Zs = [Y|Us], sorted_insert(Ys,X,Us)
    ).

insertion_sort(Xs,Ys) :- insertions(Xs,[],Ys).

insertions([],As,As).
insertions([X|Xs],As,Ss) :-
    sorted_insert(As,X,Bs), insertions(Xs,Bs,Ss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Manipulating compound terms

%% substitute(T0,X,Y,T) :-
%%     T is a copy of T0 except that each occurence of X in T0
%%     has a corresponding occurence of Y in T.
substitute(T0,X,Y,T) :-
    ( T0 == X -> T = Y
    ; compound(T0) ->
        functor(T0,F,N), functor(T,F,N),
        substitute_args(N,T0,X,Y,T)
    ; T = T0
    ).

substitute_args(N,T0,X,Y,T) :-
    ( N > 0 ->
        arg(N,T0,A), substitute(A,X,Y,B), arg(N,T,B),
        N1 is N-1, substitute_args(N1,T0,X,Y,T)
    ; true
    ).

%%% % Test:
%%% | ?- substitute(a(X,nil,b(nil,2,B,nil)),nil,[],T).
%%% T = a(X,[],b([],2,B,[]))

subterm(T,X) :-
    ( X = T
    ; compound(T), functor(T,_,N), subtermOfArgs(N,T,X)
    ).

subtermOfArgs(N,T,X) :-
    N > 0,
    ( N1 is N-1, subtermOfArgs(N1,T,X)
    ; arg(N,T,A), subterm(A,X)
    ).

%% Manipulating atomic terms

%% aA(A,B) :- B is a copy of atom A except that
%%     each lower-case letter in A is substituted 
%%     by the appropriate upper-case letter in B.
aA(A,B) :-
    atom_codes(A,Cs), cC(Cs,Ds), atom_codes(B,Ds).

cC([],[]).
cC([C|Cs],[D|Ds]) :- bB(C,D), cC(Cs,Ds).

bB(C,D) :-
    ( 0'a=<C, C=<0'z -> D is C-0'a+0'A
    ; D = C
    ).

%% Test:
%% | ?- aA('How beautiful is She!',Unknown).
%% Unknown = 'HOW BEAUTIFUL IS SHE!'

%% number_codes(A,Cs)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Extra-logical predicates
%% ------------------------

%% Interface (I/O)

%% PreCond: F1 and F2 are textfiles with appropriate access.
%% appf(F1,F2) :- the content of F2 is inserted at the end of F1.
appf(F1,F2) :- 
    open(F1,append,A), open(F2,read,R), 
    af(A,R),
    close(A), close(R).

af(A,R) :-
    get_code(R,C),
    ( C == -1 -> true       % end of R
    ; put_code(A,C), af(A,R)
    ).

%% Get an unsigned integer

getNat(S,N) :-
    get_white_spaces(S),
    peek_code(S,C),
    ( 0'0 =< C, C=<0'9 -> get_Nat(S,0,N)
    ; skip_line(S), fail
    ).

get_Nat(S,A,N) :-
    peek_code(S,C),
    ( 0'0 =< C, C =< 0'9
    -> get_code(S,_), B is 10*A+C-0'0, get_Nat(S,B,N)
    ; N = A, skip_line(S)
    ).

get_white_spaces(S) :-
    peek_code(S,C),
    ( white_space(C) -> get_code(S,_), get_white_spaces(S)
    ; true
    ).

white_space(0' ).
white_space(0'\n).
white_space(0'\t).
white_space(0'\r).


copy_prolog_file(In,Out) :-
    open(In,read,R), open(Out,write,W),
    copy_prolog_stream(R,W),
    close(R), close(W).

copy_prolog_stream(R,W) :-
    read(R,Term),
    ( Term == end_of_file -> true
    ; writeq(W,Term), write(W,'.\n'), copy_prolog_stream(R,W)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% c14nondet.pl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Searching paths:

edge(a,b).    edge(b,c).    edge(b,d).
edge(c,a).
edge(c,e).    edge(d,e).

%%             __________
%%            /          \
%%           V            \
%%           a----->b----->c
%%                  |      |
%%                  |      |
%%                  V      V
%%                  d----->e

%% path(A,Z,Path) :-
%%     Path is an acyclic list of nodes from A to Z.
path(A,Z,Path) :- path_2(A,[],Z,Path).

path_2(A,Ancestors,A,Path) :-
    reverse([A|Ancestors],Path).
path_2(A,Ancestors,Z,Path) :-
    edge(A,B), B\=A,
    \+member(B,Ancestors),
    path_2(B,[A|Ancestors],Z,Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Dynamic predicates (Extra-logical predicates again)

%% retractall/1, asserta/1, assertz/1, retract/1, abolish/1 

% Z can be reached form a given A.
reach_from(A,Z) :-
    ground(A),			% A is ground term
    %% nonvar(Z),
    retractall(visited(_)),
    asserta(visited(A)),
    reach_2(A,Z).

%% do not call directly:
reach_2(A,A).
reach_2(A,Z) :-
    edge(A,B),
    \+ visited(B),
    asserta(visited(B)),
    reach_2(B,Z).

retract_all(Head) :- retract((Head:-_Body)), fail.
retract_all(_Head).

:- dynamic(connected/2).

connected(X,X).
%% | ?- assertz((connected(X,Y):-edge(X,Z),connected(Z,Y))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate forall(:,:), collect(?,:,?), comp(:).

%% forall(P,Q) :- for each solution of P,  Q can be solved, 
%%                 that is P implies Q.
forall(P,Q) :- \+ ( P, \+Q ).

%% collect(Solution,Goal,Results) :- 
%%     Result is a strictly increasing proper list
%%            of the Solutions of Goal.
collect(Solution,Goal,Results) :-
    findall(Solution,Goal,ListOfSols), sort(ListOfSols,Results).

comp(F) :- compile(F), let_it_be_default_file(F).

comp :- default_file(F), compile(F).

let_it_be_default_file(F) :-
    retractall(default_file(_)),
    asserta(default_file(F)).

read1term(Term) :-
    catch( ( read(user,Term), skip_line(user) ) ,
           error(Err,_), ( write1term(Err), read1term(Term) )
         ).

write1term(Term) :- writeq(user,Term), nl(user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Solutions of Exercises from apl_lpp.pdf

%% 2. The facts:
family('Abraham','Sarah',['Isaac','Anon']).
family('Abraham','Hagar',['Ishmael']).
family('Isaac','Rebeka',['Jacob','Esau']).
family('Jacob','Rachel',['Joseph','Benjamin']).
family('Jacob','Leah',
       ['Reuben','Simeon','Levi','Judah','Issachar','Zebulun']).
family('Joseph','Asenath',['Manasseh','Ephraim']).

%% Based on family/3:
%%
%%  Hagar    Abraham   Sarah
%%     |------|   |-----|
%%     |                |
%%     |        |-------|
%%  Ishmael   Anon    Isaac    Rebeka
%%                      |---------|
%%                                |--------------------------|
%%                                |                          |
%%                Leah         Jacob          Rachel       Esau
%%                  |----------|   |-------------|
%%                  |                            |
%%     |-----|------|----|-------|-------|       |
%%  Reuben,Simeon,Levi,Judah,Issachar,Zebulun    |
%%                                               |---------|
%%                                 Asenath    Joseph    Benjamin
%%                                    |----------|
%%                         |----------|
%%                     Manasseh    Ephraim
%% 
brother_or_sister(X,Y) :-
    family(_,_,Xs),
    member(X,Xs), member(Y,Xs),
    X \== Y.

aunt_or_uncle(X,Y) :-
    brother_or_sister(X,Z),
    ( family(Z,_,Cs) ; family(_,Z,Cs) ),
    member(Y,Cs).

parent_in_law(X,Y) :-
    ( family(X,_,Cs) ; family(_,X,Cs) ),
    member(C,Cs),
    ( family(C,Y,_) ; family(Y,C,_) ).

%% 3. "PreCond" gives a sufficient condition
%%    of the finiteness of the search tree

%% PreCond: Xs or Ys is proper list.
%% prefix(Xs,Ys) :- Ys is a prefix of Xs.
prefix(_Xs,[]).
prefix([X|Xs],[X|Ys]) :- prefix(Xs,Ys).

%% PreCond: Xs is proper list.
%% suffix(Xs,Ys) :- Ys is a suffix of Xs.
suffix(Xs,Xs).
suffix([_X|Xs],Ys) :- suffix(Xs,Ys).

%% PreCond: Xs is proper list.
%% sublist_x(Xs,Ys) :- Ys is a continuous sublist of Xs.

%% a: Suffix of a prefix
sublist_a(Xs,Ys) :- prefix(Xs,Ps), suffix(Ps,Ys).

%% b: Prefix of a suffix
sublist_b(Xs,Ys) :- suffix(Xs,Ss), prefix(Ss,Ys).

%% c: Recursive definition of a sublist
sublist_c(Xs,Ys) :- prefix(Xs,Ys).
sublist_c([_X|Xs],Ys) :- sublist_c(Xs,Ys).

%% PreCond: Xs is proper list.
%% subseq(Xs,Ys) :-
%%     Ys is a possibly discontinuous subsequence of Xs.
subseq([],[]).
subseq([X|Xs],[X|Ys]) :- subseq(Xs,Ys).
subseq([_X|Xs],Ys) :- subseq(Xs,Ys).

%% PreCond: Xs is proper list.
%% divide(Xs,Odds,Evens) :- in their original order,
%%     the first, third, fifth, etc. items of Xs are in Odds,
%%     the second, fourth, sixth, etc. items  of Xs are in Evens.
divide([],[],[]).
divide([X|Xs],[X|Ys],Zs) :- divide(Xs,Zs,Ys).

%% PreCond: Xs and Ys are proper lists, sorted strictly 
%%          increasingly according to the standard order.
%% sorted_union(Xs,Ys,Us) :-
%%     Us contains the sorted union of Xs and Ys.

sorted_union([],Ys,Ys).
sorted_union([X|Xs],[],Us) :- !, Us = [X|Xs].
sorted_union([X|Xs],[Y|Ys],Us) :-
    ( X @< Y -> Us = [X|Zs], sorted_union(Xs,[Y|Ys],Zs)
    ; X @> Y -> Us = [Y|Zs], sorted_union([X|Xs],Ys,Zs)
    ; Us = [X|Zs], sorted_union(Xs,Ys,Zs)
    ).

%% 4.
%% PreCond: Tree is a proper binary tree repersented as follows. 
%%     o - empty tree
%%     t( LeftSubTree, Root, RightSubTree ) - nonempty tree
inorder(o,[]).
inorder(t(Lt,X,Rt),Xs) :-
    inorder(Lt,Ls), inorder(Rt,Rs), append_(Ls,[X|Rs],Xs).

%% The optimized version of predicate inorder/2. 
%% Without appends, linear time complexity.
inorder_opt(T,Is) :- inorder_app(T,[],Is).

%% inorder_app(Tree,List,Is) :- 
%%     the inorder traversal of Tree
%%     appended before List results Is.
inorder_app(o,Xs,Xs).
inorder_app(t(Lt,X,Rt),Xs,Is) :-
    inorder_app(Rt,Xs,Ys), inorder_app(Lt,[X|Ys],Is).

%% 4. The usual operations on proper binary search trees.
empty_tree(o).

tree_ins(o,X,t(o,X,o)).
tree_ins(t(Lt,Root,Rt),X,TX) :-
    ( X @< Root -> TX = t(LXt,Root,Rt), tree_ins(Lt,X,LXt)
    ; X @> Root -> TX = t(Lt,Root,RXt), tree_ins(Rt,X,RXt)
    ; TX = t(Lt,Root,Rt)
    ).

%% Check for an item.
tree_has(t(Lt,Root,Rt),X) :-
    ( X @< Root -> tree_has(Lt,X)
    ; X @> Root -> tree_has(Rt,X)
    ; true
    ).

tree_del(t(Lt,Root,Rt),X,T) :-
    ( X @< Root -> T = t(Ldt,Root,Rt), tree_del(Lt,X,Ldt)
    ; X @> Root -> T = t(Lt,Root,Rdt), tree_del(Rt,X,Rdt)
    ; Rt == o -> T = Lt
    ; Lt == o -> T = Rt
    ; T = t(Lt,Min,Rmt), Rt = t(L,Y,R), out_min(L,Y,R,Min,Rmt)
    ).

%% out_min(L,X,R,Min,Tm) :-
%%     the leftmost element of the proper binary tree
%%     t(L,X,R) is Min, the remaining tree is Tm.
out_min(o,Min,Rt,Min,Rt).
out_min(t(L,X,R),Y,Rt,Min,t(Lm,Y,Rt)) :-
    out_min(L,X,R,Min,Lm).

%% 5.
insertsort(Xs,Ys) :- sorted_inserts(Xs,[],Ys).

%% sorted_inserts(Xs,As,Ys) :- sorted insert of 
%%     the items of proper list Xs into the sorted
%%     proper list As results sorted proper list Ys.
sorted_inserts([],Ys,Ys).
sorted_inserts([X|Xs],As,Ys) :-
    insert_sorted(As,X,Bs), sorted_inserts(Xs,Bs,Ys).

%% insert_sorted(As,X,Bs) :- sorted insert of X
%%               into the sorted proper list As 
%%               results sorted proper list Ys.
insert_sorted([],X,[X]).
insert_sorted([Y|Ys],X,Zs) :- 
    ( X @=< Y -> Zs = [X,Y|Ys]
    ; Zs = [Y|Us], insert_sorted(Ys,X,Us)
    ).

mergesort([],[]).
mergesort([X|Xs],Ys) :- mergesort(Xs,X,Ys).

mergesort([],X,[X]).
mergesort([Y|Ys],X,Zs) :-
    divide(Ys,As,Bs),
    mergesort(As,X,Es), mergesort(Bs,Y,Fs),
    sorted_merge(Es,Fs,Zs).

sorted_merge([],Ys,Ys).
sorted_merge([X|Xs],[],Ms) :- !, Ms = [X|Xs].
sorted_merge([X|Xs],[Y|Ys],Ms) :-
    ( X @< Y -> Ms = [X|Zs], sorted_merge(Xs,[Y|Ys],Zs)
    ; X @> Y -> Ms = [Y|Zs], sorted_merge([X|Xs],Ys,Zs)
    ; Ms = [X,Y|Zs], sorted_merge(Xs,Ys,Zs)
    ).

%% Note: if we use sorted_union/3 instead of sorted_merge/3,
%% we receive a strictly increasing list, duplicates removed:

unionsort([],[]).
unionsort([X|Xs],Ys) :- unionsort(Xs,X,Ys).

unionsort([],X,[X]).
unionsort([Y|Ys],X,Zs) :-
    divide(Ys,As,Bs),    % See exercise 3.
    unionsort(As,X,Es), unionsort(Bs,Y,Fs),
    sorted_union(Es,Fs,Zs).

%% 6.
%% ground_(Term) :- ground(Term).

ground_(Term) :-
    ( atomic(Term) -> true
    ; compound(Term),
      functor(Term,_F,N), ground_args(N,Term)
    ).

ground_args(N,Term) :- N>1,
    arg(N,Term,A), ground_(A),
    N1 is N-1, ground_args(N1,Term).
ground_args(1,Term) :-
    arg(1,Term,A), ground_(A).

%% 7.
%% :- module( findall, [ find_all/3 ] ).
:- meta_predicate find_all(?,:,?).

:- dynamic((solution/2, counter/1)).

set_counter(C) :-
    retractall(counter(_)),
    asserta(counter(C)).

:- set_counter(1).

find_all(X,Goal,Xs) :-
    counter(I),
    %% to handle embedded calls to find_all/3:
    I1 is I+1, set_counter(I1),
    catch(
	  (
	    %% clear things, if an old goal crashed:
	    retractall(solution(I,_)),
	    %% assert solutions at the Ith level:
	    Goal, assertz(solution(I,X)), fail
	  ;
	    collect_solutions(I,Xs),
	    set_counter(I)
	  ),
	  Error,
	  ( set_counter(I),
	    throw(Error)
	  )
	 ).

collect_solutions(I,Ys) :-
    ( retract(solution(I,X)) ->
          Ys = [X|Xs], collect_solutions(I,Xs)
    ; Ys = []
    ).
%% | ?- find_all( Zs+Ys,
%%                ( append(Xs,Ys,[1,2,3]),
%%                  find_all(Z,member(Z,Xs),Zs) ), As).
%% As = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]]

%% 8. Queues:

%% proper list representation:
%% A queue has the form: [X1,X2,...,XN]

empty0([]).

%% add0/3 has linear computational complexity:

add0([],X,[X]).
add0([Y|Ys],X,[Y|Zs]) :- add0(Ys,X,Zs).

rem0([X|Xs],X,Xs).

%% double-stack representation:
%% d([X1,...,XM],[YN,...,Y1]) represents
%% the abstract queue <X1,...,XM,Y1,...,YN>

empty(d([],[])).

add(d(Xs,Ys),E,d(Xs,[E|Ys])).

rem(d(Xs,Ys),E,ResultQueue) :-
    ( Xs = [Z|Zs] -> E = Z, ResultQueue = d(Zs,Ys)
    ; %% Xs == [], Ys \== [],
      reverse(Ys,[E|Us]), ResultQueue = d(Us,[])
    ).

%% The computational complexity of the reverse call above is linear, but Ys is as long as the number of add calls before the actual reverse call form rem (but not before any previous reverse call form rem). Therefore the cost of the actual reverse call can be scattered among those add calls when we calculate the average computational complexity of add and rem, and so it is $\Theta(1)$.

%% Queue represented with a pair of partial lists
%% where the second component is the logical variable
%% at the end of the first partial list:
%% a nonempty queue has this form: [P1,P2,...,PN|Z]-Z
%% ([P1,P2,...,PN|Z] is the minuend and Z is the subtrahend,
%% but no difference is calculated. ) 
%% An empty queue has this form: Z-Z
%% where var(Z) must be true.

%% In general Xs-Ys is a d-list (i.e. difference list), if
%% Xs and Ys are (possibly partial) lists, and Ys is suffix of Xs.

%% emptyq(Queue):- Queue is an empty queue.
emptyq(Q-Q) :- var(Q).

%% addq(InQ,ITEM,ResQ):- d-list (queue) InQ + ITEM
%%    results d-list (queue) ResQ with a var subtrahend.
%% PreCond: InQ is a d-list with a var subtrahend. ResQ is a var.
addq(Q1-[ITEM|Y],ITEM,Q1-Y).
%% A call: addq([P1,P2,...,PN|Z]-Z,ITEM,R).
%% After the call: Q1 = [P1,P2,...,PN|Z],
%%                 Z = [ITEM|Y], R = Q1-Y.
%% Thus Q1 = [P1,P2,...,PN|[ITEM|Y]] = [P1,P2,...,PN,ITEM|Y]
%% Consequently R = [P1,P2,...,PN,ITEM|Y]-Y.

remq([H|T]-Z,H,T-Z) :- 
     [H|T]\==Z.  % the queue was nonempty


%%% \item { \refstepcounter{lpsol} \label{lp:solutions:bfs} }

%% Graph-search with breadth-first-search strategy.

%% breadth_first_search(Start,Goal,SolPath) :-
%%     SolPath is a proper list representing a path of
%%     the minimal length (i.e.optimal) from Start to Goal.
breadth_first_search(Start,Goal,SolPath) :-
    ground(Start), ground(Goal),
    ( Start == Goal -> SolPath = [Start]
    ; emptyq(InitQueue), addq(InitQueue,[Start],Queue), 
      bfs(Queue,[Start],Goal,SolPath)
    ).

%% Queue is the queue of lists of the form [Node|Ancestors] 
%% where Node is a node to which we have found the optimal
%% path but has not been expanded. Ancestors consists of
%% the ancestors of Node on this optimal path starting
%% with its parent. Visited contains the visited nodes.
bfs(Queue,Visited,Goal,SolPath) :- 
    remq(Queue,[Node|Ancestors],RemainderQueue), 
    children(Node,Children,Visited),
    ( has(Children,Goal) ->
          reverse([Goal,Node|Ancestors],SolPath)
    ; process_children(Children,[Node|Ancestors],
		   RemainderQueue,ResultQueue), 
      append(Children,Visited,NewVisited),
      bfs(ResultQueue,NewVisited,Goal,SolPath) 
    ).

%% expansion: Children is the list of
%%     the nonvisited children of Node.
children(Node,Children,Visited) :- 
    findall(Child,child(Node,Child,Visited),Children).

%% Child is a nonvisited child of Node.
child(Node,Child,Visited) :- 
    arc(Node,Child), \+ has(Visited,Child).

has([X|Xs],Y) :-
    ( X == Y -> true
    ; has(Xs,Y)
    ).

%% Add the children with their ancestors to RemainderQueue. 
process_children([FirstChild|Children],Ancestors,
	     RemainderQueue,ResultQueue) :- 
    addq(RemainderQueue,[FirstChild|Ancestors],TempQueue), 
    process_children(Children,Ancestors,TempQueue,ResultQueue).
process_children([],_Ancestors,ResultQueue,ResultQueue).

/*  Data   */

%% Acyclic component
    arc(a,b).   arc(a,c).   arc(a,d).  arc(a,e).  arc(d,j).  
    arc(c,f).   arc(c,g).   arc(f,h).  arc(e,k).  arc(f,i).  
    arc(j,g).   arc(g,h).   arc(k,j).  arc(b,f).  arc(b,i).

/*
     a------>b-------->i
    /|\       \      /
   | | \       \    /                        __________
   | |  \       \  /                        /          \
   | V   V       V/                        V            \
   | d   c------>f---->h                   x----->y----->z
   |  \   \          /                            |      |
   |   \   \        /                             |      |
   |    \   \      /                              V      V
   |     \   \    /                               u      t
   |      \   \  /
   V       V   V/
   e-->k-->j-->g
*/

%% Cyclic component
    arc(x,y).   arc(y,z).   arc(z,x).  arc(y,u).  arc(z,t).

%% | ?- breadth_first_search(a,g,Sol).
%% Sol = [a,c,g]
%% | ?- breadth_first_search(a,i,Sol).
%% Sol = [a,b,i]
%% | ?- breadth_first_search(x,t,Sol).
%% Sol = [x,y,z,t]
