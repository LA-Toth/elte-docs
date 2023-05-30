%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Chapter 14.: Nondeterminstic Programming (Generate and Test)    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(100)]).

:- use_module( library(lists), [ reverse/2 ] ).

/*
verb(Sentence,Verb) :-
    Verb is a verb in the list of words Sentence.
*/

verb(Sentence,Word) :- member(Word,Sentence), verb(Word).
noun(Sentence,Word) :- member(Word,Sentence), noun(Word).
article(Sentence,Word) :- member(Word,Sentence), article(Word).

/* Vocabulary */

    noun(man).                  noun(woman).
    article(a).                 verb(loves).

%% Program 14.1: Finding parts of speech in a sentence

%%% | ?- noun([a,man,loves,a,woman],Word).
%%% Word = man ? ;
%%% Word = woman ? ;
%%% no
%%% | ?- verb([a,man,loves,a,woman],Word).
%%% Word = loves ? ;
%%% no

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   nqueens(N,Queens) :-
     Queens is a placement that solves the N queens problem,
     represented as a permutation of the list of numbers [1,2,...,N].
*/

%%% | ?- nqueens(4,Qs).
%%% Qs = [2,4,1,3] ? ;
%%% Qs = [3,1,4,2] ? ;
%%% no

%%% - - Q -       - Q - -
%%% Q - - -       - - - Q
%%% - - - Q       Q - - -
%%% - Q - -       - - Q -

nqueens(N,Qs) :- range(1,N,Ns), permutation(Ns,Qs), safe(Qs).

/*
safe(Qs) :- The placement Qs is safe.
*/     

safe([Q|Qs]) :- safe(Qs), \+ attack(Q,Qs).
safe([]).     
     
attack(X,Xs) :- attack(X,1,Xs).

attack(X,N,[Y|_]) :- N =:= abs(X-Y).
attack(X,N,[_|Ys]) :- N1 is N+1, attack(X,N1,Ys).

% Homework: Analyse permutation/2 and range/3.

permutation(Xs,[Z|Zs]) :- select(Z,Xs,Ys), permutation(Ys,Zs).
permutation([],[]).

select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

%% Precond.: M, N are integers.
range(M,N,[M|Ns]) :- M < N, !, M1 is M+1, range(M1,N,Ns).
range(N,N,[N]).
%% range(N,_,[N]). %% Precond.: M, N are integers and M=<N.

%	Program 14.2: Naive generate-and-test program solving N queens

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The solution above is similar to the following way of sorting.
%% Worst and average case performance: Theta(N!*N) where N is the length of the list.

perm([],[]).
perm([X|Xs],Ys) :- perm(Xs,Zs), nondet_ins(Zs,X,Ys).

nondet_ins(Xs,X,[X|Xs]).
nondet_ins([Y|Zs],X,[Y|Ys]) :- nondet_ins(Zs,X,Ys).

perm_sort(Xs,Ys) :- perm(Xs,Ys), sorted(Ys).
%%        simple    generate and test

sorted([]).
sorted([_X]) :- !.
sorted([X,Y|Ys]) :- X=<Y, sorted([Y|Ys]).


%% More efficient:
%% Worst and average case performance: Theta(N*N) where N is the length of the list.

insertionSort0([],[]).
insertionSort0([X|Xs],Ys) :- insertionSort0(Xs,Zs), sorted_ins(Zs,X,Ys).
%%                           generate recursively,  testing is included here

%% Only sorted partial solutions are generated.
sorted_ins([],X,[X]).
sorted_ins([Y|Xs],X,[X,Y|Xs]) :- X=<Y, !.
sorted_ins([Y|Zs],X,[Y|Ys]) :- X>Y, sorted_ins(Zs,X,Ys).

%% Insertion sort is approximately (N-1)! times faster than permutation sort.

%% Still we can develop Insertion sort by taking advantage of
%%   first-argument indexing and
%%   tail recursion optimization:

insertionSort(Xs,Ys) :- sortedInserts(Xs,[],Ys).

%% We use accumulator technics here:
sortedInserts([],Sorted,Sorted).
sortedInserts([X|Xs],Sorted,Result) :-
    sortedIns(Sorted,X,SortedWithX), sortedInserts(Xs,SortedWithX,Result).

%% Step-by-step approximation of the output (*):
sortedIns([],X,[X]).
sortedIns([Y|Xs],X,Rs) :-
    ( X<Y -> Rs = [X,Y|Xs]
    ; Rs = [Y|Zs], sortedIns(Xs,X,Zs) %% (*)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
queens(N,Queens)  :-
    Queens is a placement that solves the N queens problem,
    represented as a permutation of the list of numbers [1,2,..,N].
*/

%%% | ?- queens(4,Qs).
%%% Qs = [3,1,4,2] ? ;
%%% Qs = [2,4,1,3] ? ;
%%% no

%%% - - Q -       - Q - -
%%% Q - - -       - - - Q
%%% - - - Q       Q - - -
%%% - Q - -       - - Q -

% Notice that the generate steps and the test steps are merged into a 
% recursive procedure. Only safe partial solutions are generated.

queens(N,Qs) :- range(1,N,Ns), queens(Ns,[],Qs).

queens(UnplacedQs,SafeQs,Qs) :-
    select(Q,UnplacedQs,UnplacedQs1), 
    \+ attack(Q,SafeQs), 
    queens(UnplacedQs1,[Q|SafeQs],Qs).
queens([],Qs,Qs).

/*
range(I,N,[I|Ns]) :- I < N, I1 is I+1, range(I1,N,Ns).
range(N,N,[N]).

select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

attack(X,Xs) :- attack(X,1,Xs).

attack(X,N,[Y|Ys]) :- X is Y+N ; X is Y-N.
attack(X,N,[Y|Ys]) :- N1 is N+1, attack(X,N1,Ys).
*/

%	Program 14.3: Placing one queen at a time

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
color_map(Map,Colors) :-
    Map is colored with Colors, so that no two neighbors have the same
    color. The map is represented as an adjacency-list of regions
    region(Name,Color,Neighbors), where Name is the name of the region,
    Color is its color, and Neighbors are the colors of the neighbors. 
*/

color_map([Region|Regions],Colors) :-
    color_region(Region,Colors),
    color_map(Regions,Colors).
color_map([],_).

/*
color_region(Region,Colors) :-
    Region and its neighbors are colored using Colors so that the
    region's color is different from the color of any of its neighbors.
*/

color_region(region(_,Color,Neighbors),Colors) :-
    select(Color,Colors,Colors1),
    members(Neighbors,Colors1).

%	select(X,[X|Xs],Xs).
%	select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

members([X|Xs],Ys) :- member(X,Ys), members(Xs,Ys).
members([],_).

%	Program 14.4: Map coloring


/* Test data */

test_color(Name,Map) :-
    map(Name,Map),
    colors(Name,Colors),
    color_map(Map,Colors).

map( test, [ region(a,A,[B,C,D]),     region(b,B,[A,C,E]), 
	     region(c,C,[A,B,D,E,F]), region(d,D,[A,C,F]),
	     region(e,E,[B,C,F]),     region(f,F,[C,D,E])
	   ]
   ).

%% map test, first solution:
%%
%%% --------------------------------------
%%% |              a : red               |
%%% --------------------------------------
%%% | b : yellow | c : blue | d : yellow |
%%% --------------------------------------
%%% |     e : red     |     f : white    |
%%% --------------------------------------

map( west_europe,
     [ region(portugal,P,[E]),  region(spain,E,[F,P]),
       region(france,F,[E,I,S,B,WG,L]),
       region(belgium,B,[F,H,L,WG]),
       region(holland,H,[B,WG]),
       region(west_germany,WG,[F,A,S,H,B,L]),
       region(luxembourg,L,[F,B,WG]),
       region(italy,I,[F,A,S]),
       region(switzerland,S,[F,I,A,WG]),
       region(austria,A,[I,S,WG])
     ]
   ).

map( ch,
     [ region(a,A,[B]),
       region(b,B,[A,C,L]),
       region(c,C,[B,D,L]),
       region(d,D,[C,K,L]),
       region(e,E,[D,K,J,F]),
       region(f,F,[J,E,L,I,G]),
       region(g,G,[H,I,F]),
       region(i,I,[H,G,F,L]),
       region(h,H,[L,I,G]),
       region(j,J,[L,F,E,K]),
       region(k,K,[L,J,E,D]),
       region(l,L,[H,I,F,J,K,D,C,B]),
       region(o,_O,[])
     ]
   ).

colors(_,[red,yellow,blue,white]).

%	Program 14.5: Test data for map coloring

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
solve_puzzle(Puzzle,Solution) :-
    Solution is a solution of Puzzle,
    where Puzzle is puzzle(Clues,Queries,Solution).
*/

solve_puzzle(puzzle(Clues,Queries,Solution),Solution) :-
    solve(Clues),
    solve(Queries).

solve([Clue|Clues]) :-
    Clue, solve(Clues).
solve([]).

%	Program 14.6: A puzzle solver


	/* Test data */
/*
  Three friends came first second and third in a programming competition.
Each of the three has a different first name, likes a different sport, 
and has a different nationality.
  Michael likes basketball and did better than the American. Simon, the 
Israeli did better than the tennis player. The cricket player came first.
  Who is the Australian? What sport does Richard play?
*/

test_puzzle(Name,Solution) :-
    structure(Name,Structure),
    clues(Name,Structure,Clues),
    queries(Name,Structure,Queries,Solution),
    solve_puzzle(puzzle(Clues,Queries,Solution),Solution).

% A term of the form friend(Name,Nationality,Sport) represents a friend.
% The whole list represents the three friends, 
%                the winner comes first and so on.

structure( friends,
	   [ friend(_,_,_), friend(_,_,_), friend(_,_,_) ] ).

clues( friends,Friends,
       [ ( did_better(Man1Clue1, Man2Clue1, Friends),          % Clue 1
	   name_(Man1Clue1, michael), sport(Man1Clue1,basketball),
	   nationality(Man2Clue1,american)
	 ),
	 ( did_better(Man1Clue2, Man2Clue2, Friends),          % Clue 2
	   name_(Man1Clue2, simon), nationality(Man1Clue2,israeli),
	   sport(Man2Clue2,tennis)
	 ),
	 ( first(Friends,ManClue3), sport(ManClue3,cricket)    % Clue 3
	 )
       ]
     ).

queries( friends, Friends,
	 [ member(Q1,Friends),
	   name_(Q1,Name),
	   nationality(Q1,australian),                   % Query 1
	   member(Q2,Friends),
	   name_(Q2,richard),
	   sport(Q2,Sport)			         % Query 2
	 ],
	 [ ['The Australian is', Name], ['Richard plays ', Sport] ]
       ).

% did_better(A,B,[A|Ys]) :- member(B,Ys).
% did_better(A,B,[_|Ys]) :- did_better(A,B,Ys).

did_better(A,B,[A,B,_]).
did_better(A,C,[A,_,C]).
did_better(B,C,[_,B,C]).

name_(friend(A,_,_),A).
nationality(friend(_,B,_),B).
sport(friend(_,_,C),C).

first([X|_],X).

%	Program 14.7: A description of a puzzle

% Disadvantage: solve_puzzle/2 forces meta-calls, that is,
				% interpreted calls.
% It can be avoided by program transformation:

puzzle( Name, Solution ) :-
    structure( Name, Structure ),
    clues( Name, Structure ),
    queries( Name, Structure, Solution ).

%       structure( friends,
%	   [ friend(N1,C1,S1), friend(N2,C2,S2), friend(N3,C3,S3) ] ).

clues( friends, Friends ) :-
    did_better( Man1Clue1, Man2Clue1, Friends ),              % Clue 1
    name_( Man1Clue1, 'Michael' ), sport( Man1Clue1,basketball ),
    nationality( Man2Clue1, 'American' ),
    did_better( Man1Clue2, Man2Clue2, Friends ),              % Clue 2
    name_( Man1Clue2, 'Simon' ), nationality( Man1Clue2, 'Israeli' ),
    sport( Man2Clue2, tennis ),
    first( Friends, ManClue3 ), sport( ManClue3, cricket ).   % Clue 3

queries( friends, Friends,
	 [ [ 'The Australian is', Name ], [ 'Richard plays ', Sport ] ]
       ) :-
    member( Q1, Friends ),
    name_( Q1, Name ),
    nationality( Q1, australian ),                % Query 1
    member( Q2, Friends ),
    name_( Q2, richard ),
    sport( Q2, Sport ).			          % Query 2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
connected(X,Y) :-
		Node X is connected to node Y,
		given an edge/2 relation describing a DAG.
                ( DAG = Directed Acyclic Graph )
*/

dag_connected(X,X).
dag_connected(X,Y) :- edge(X,N), dag_connected(N,Y).

%	Program 14.8: Connectivity in a finite DAG


  /*  Data   */

% Acyclic component
    edge(a,b).   edge(a,c).   edge(a,d).  edge(a,e).  edge(d,j).  
    edge(c,f).   edge(c,g).   edge(f,h).  edge(e,k).  edge(f,i).  
    edge(j,g).   edge(g,h).   edge(k,j).  edge(b,f).  edge(b,i).

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

% Cyclic component
      edge(x,y).   edge(y,z).   edge(z,x).  edge(y,u).  edge(z,t).




/*  dag_path(X,Y,Path) :-
		Path is a path between two nodes X and Y in the
		DAG defined by the relation edge/2
*/
dag_path(X,X,[X]).
dag_path(X,Y,[X|Ps]) :- edge(X,N), dag_path(N,Y,Ps).

%	Program 14.9: Finding a path in a DAG by depth-first search



/*
   connected(X,Y) :-
	Node X is connected to node Y in the graph defined by edge/2.
*/	

connected(X,Y) :- connected(X,Y,[X]).

connected(X,X,_).
connected(X,Y,Visited) :- 
    edge(X,N), \+ member(N,Visited), connected(N,Y,[N|Visited]).

%	Program 14.10a: Connectivity in a graph



% Homework: Write your own predicate path/3 (for any DG)
%                 merging program 14.9 and program 14.10a.
%      Compare it with the next solutions for finding a path in a DG:



/*  dg_path(X,Y,Path) :-
		Path is a path between two nodes X and Y in the
		graph defined by the relation edge/2
*/

dg_path(X,Y,Path) :- path_(X,Y,[X],Path).

path_(X,X,Visited,Path) :- reverse(Visited,Path).
path_(X,Y,Visited,Path) :- 
    edge(X,N), \+ member(N,Visited), path_(N,Y,[N|Visited],Path).

%	Program 14.10b: Finding a path by depth-first search



/*  path(X,Y,Path) :-
	Path is a path between two nodes X and Y in the
	graph defined by the relation edge/2, because \+edge(Z,Z).
*/

path(X,Y,Path) :- path(X,[],Y,Path).

path(X,Ancestors,X,Path) :- reverse([X|Ancestors],Path).
path(X,Ancestors,Y,Path) :- 
    edge(X,N), \+ member(N,Ancestors), path(N,[X|Ancestors],Y,Path).

%	Program 14.10c: Finding a path by depth-first search
%                       in a DG not containing loop-edge.

% Homework: How to rewrite the programs in 14.8--14.10c, 
%           if the graph is not directed?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    transform(State1,State2,Plan) :-
	Plan is a plan of actions to transform State1 into State2.
*/

transform(State1,State2,Plan) :- 
   transform(State1,State2,[State1],Plan).

transform(State,State,_,[]).
transform(State1,State2,Visited,[Action|Actions]) :-
    legal_action(Action,State1),
    update(Action,State1,State), 
    \+ member(State,Visited),
    transform(State,State2,[State|Visited],Actions).

% Blocks and places are given.
% Actions: to_place(Block,Y,Place): Put Block from Y to Place.
%          to_block(Block1,Y,Block2): Put Block1 from Y onto Block2.
% Note: Y can be a place or a block.

legal_action(to_place(Block,Y,Place),State) :- 
    on(Block,Y,State), clear(Block,State),
    place(Place), clear(Place,State).
legal_action(to_block(Block1,Y,Block2),State) :- 
    on(Block1,Y,State), clear(Block1,State),
    block_(Block2), 
    Block1 \== Block2, clear(Block2,State).

clear(X,State) :- \+ member(on(_,X),State).
on(X,Y,State) :- member(on(X,Y),State).

update(to_block(X,Y,Z),State,State1) :-
    substitute(on(X,Y),on(X,Z),State,State1).
update(to_place(X,Y,Z),State,State1) :-
    substitute(on(X,Y),on(X,Z),State,State1).

substitute(X,Y,[X|Xs],Zs) :- !, Zs = [Y|Xs].
substitute(X,Y,[X1|Xs],[X1|Ys]) :- X \= X1, substitute(X,Y,Xs,Ys).

%	Program 14.11a: A depth-first planner

:- discontiguous initial_state/2, final_state/2.

  /*  Testing and data  */

plan(Name,Plan) :-
    initial_state(Name,I), final_state(Name,F), transform(I,F,Plan).

initial_state(test,[on(a,b),on(b,p),on(c,r)]).
final_state(test,[on(a,b),on(b,c),on(c,r)]).

  block_(a).	block_(b).	block_(c).    block_(d).
  place(p).	place(q).	place(r).

%	Program 14.11b: Testing the depth-first planner

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% plan(Name,D,Plan) <- Plan is a solution of problem Name 
%                      and its length is =< D.

plan(Name,D,Plan) :-
    initial_state(Name,I), final_state(Name,F), 
    transform(I,F,Plan),
    length(Plan,L), L=<D.

initial_state(t2,[on(a,b),on(b,p)]).
final_state(t2,[on(a,b),on(b,r)]).

% Homework: Write efficient code for introducing depth-limit.

%	Program 14.12a: A naive solution for introducing depth-limit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A solution to the question in program 14.12a:

plan_(Name,D,Plan) :-
   initial_state(Name,I), final_state(Name,F), 
   transform_(I,F,D,Plan).

transform_(State1,State2,D,Plan) :- 
   transform_(State1,State2,D,[State1],Plan).

transform_(State,State,_,_,[]).
transform_(State1,State2,D,Visited,[Action|Actions]) :-
   D > 0, D1 is D-1,
   legal_action(Action,State1),
   update(Action,State1,State), 
   \+ member(State,Visited),
   transform_(State,State2,D1,[State|Visited],Actions).

%	Program 14.12b: A better solution for introducing depth-limit

plan_safe(Name,DMax,Plan) :-
   integer(DMax), 
   initial_state(Name,I), final_state(Name,F), 
   genD(0,D,DMax),
   transform_(I,F,D,Plan), !.

genD(I1,D,DMax) :-
    I1 =< DMax, 
    ( D = I1
    ; I2 is I1+1, genD(I2,D,DMax)
    ).

initial_state(t0,[on(a,b),on(b,c),on(c,p)]).
final_state(t0,[on(a,b),on(b,c),on(c,p)]).

initial_state(t3,[on(a,b),on(b,c),on(c,p)]).
final_state(t3,[on(a,b),on(b,c),on(c,r)]).

initial_state(t4,[on(a,b),on(b,c),on(c,d),on(d,p)]).
final_state(t4,[on(a,b),on(b,c),on(c,d),on(d,r)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
