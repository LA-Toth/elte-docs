:- module(bfs,[breadth_first_search/3,
	       breadth_first_planner/2,
	       two_containers/4,two_container_test/3]).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(100)]).

%%  Breadth-first-search: 

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
    %% edge(t,x).
    edge(x,y).   edge(y,z).   edge(z,x).  edge(y,u).  edge(z,t).  

%% Graph-search with breadth-first-search strategy.

%% breadth_first_search(Start,Goal,SolPath) :-
%%     SolPath is a proper list representing a path of
%%     the minimal length (i.e.optimal) from Start to Goal.
breadth_first_search(Start,Goal,SolPath) :-
    ground(Start), ground(Goal),
    ( Start == Goal -> SolPath = [Start]
    ; empty(InitQueue), add(InitQueue,[Start],Queue), 
      bfs(Queue,[Start],Goal,SolPath)
    ).

%% Queue is the queue of lists of the form [Node|Ancestors] 
%% where Node is a node to which we have found the optimal
%% path but has not been expanded. Ancestors consists of
%% the ancestors of Node on this optimal path starting
%% with its parent. Visited contains the visited nodes.
bfs(Queue,Visited,Goal,SolPath) :- 
    rem(Queue,[Node|Ancestors],RemainderQueue), 
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
    edge(Node,Child), \+ has(Visited,Child).

has([X|Xs],Y) :-
    ( X == Y -> true
    ; has(Xs,Y)
    ).

%% Add the children with their ancestors to RemainderQueue. 
process_children([FirstChild|Children],Ancestors,
		 InputQueue,ResultQueue) :- 
    add(InputQueue,[FirstChild|Ancestors],TempQueue), 
    process_children(Children,Ancestors,TempQueue,ResultQueue).
process_children([],_Ancestors,ResultQueue,ResultQueue).

:- use_module(library(lists),[reverse/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Queue handling 1:

%% Naive solution, i.e.
%% proper list representation:
%% A queue has the form: [X1,X2,...,XN]
%% [] represents the empty queue.

%%% empty([]).

%%% add([],X,[X]).
%%% add([Y|Ys],X,[Y|Zs]) :- add(Ys,X,Zs).

%%% rem([X|Xs],X,Xs).

%% PROBLEM: add(Q1,X,Q2) has linear computational complexity.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Queue handling 2:

%% The average runtime of the queue operations is Theta(1).

%% double-stack representation:
%% d([X1,...,XM],[YN,...,Y1]) represents
%% the abstract queue <X1,...,XM,Y1,...,YN>

%%% empty(q([],[])).

%%% add(q(Xs,Ys),E,q(Xs,[E|Ys])).

%%% rem(q(Xs,Ys),E,q(Us,Vs)) :-
%%%     ( Xs==[] -> reverse(Ys,[E|Us]), Vs=[]
%%%     ; Xs=[E|Us], Ys=Vs
%%%     ).

%% This is better solution, because the average runtime of the add and rem calls is Theta(1):
%% The computational complexity of the reverse call above is linear.
%% But Ys is as long as the number of add calls which are before the actual reverse call but not before any previous reverse call.
%% Therefore the runtime cost of the actual reverse call can be scattered among those add calls when we calculate the average computational complexity of the add and rem calls. (This is amortized analysis of runtime.)
%% Thus the average time complexity of the add and rem calls is Theta(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%% Queue handling 3 (with d-lists): %%%%%%%%%%%%

%% The runtime of each queue op. is Theta(1). (No slow operation.)

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

%% Note: Important applications of d-lists are logic grammars.
%% (And logic grammars are useful in processing natural and computer languages.)

%% D-list Xs-Ys is a representation of the proper list received by
%% substituting the Ys suffix of Xs with [].

%% For example:
%% [P1,P2,...,PN|Z]-Z represents [P1,P2,...,PN]
%% [P1,P2,...,PN,Q1,...,QM|Z]-[Q1,...,QM|Z] also represents [P1,P2,...,PN]
%% [P1,P2,...,PN,Q1,...,QM]-[Q1,...,QM] also represents [P1,P2,...,PN]
%% [P1,P2,...,PN]-[] also represents [P1,P2,...,PN]
%% Zs-Zs represents [] where Zs is a (possibly partial) list.

%%% concat(X-Y,Y-Z,X-Z).  %% d-list concatenation in constant time.
%% A call: concat([A1,A2,...,AM|AT]-AT,[B1,B2,...,BN|BT]-BT,AB).
%% We suppose that var(AT).
%% After the call: X = [A1,A2,...,AM|AT], AT = Y,
%%                 Y = [B1,B2,...,BN|BT], Z = BT,
%%                 AB = X-Z = X-BT.
%% Thus X = [A1,A2,...,AM|[B1,B2,...,BN|BT]] = [A1,A2,...,AM,B1,B2,...,BN|BT]
%% Consequently AB = [A1,A2,...,AM,B1,B2,...,BN|BT]-BT.

%% Back to queues (each queue op. runs in Theta(1) time):

%% empty(Queue):- Queue is an empty queue.
empty(Q-Q) :- var(Q).

%% addq(InQ,ITEM,ResQ):- d-list (queue) InQ + ITEM
%%    results d-list (queue) ResQ with a var subtrahend.
%% PreCond: InQ is a d-list with a var subtrahend. ResQ is a var.
add(Q1-[ITEM|Y],ITEM,Q1-Y).
%% A call: addq([P1,P2,...,PN|Z]-Z,ITEM,R).
%% After the call: Q1 = [P1,P2,...,PN|Z],
%%                 Z = [ITEM|Y], R = Q1-Y.
%% Thus Q1 = [P1,P2,...,PN|[ITEM|Y]] = [P1,P2,...,PN,ITEM|Y]
%% Consequently R = [P1,P2,...,PN,ITEM|Y]-Y.

rem([H|T]-Z,H,T-Z) :- 
     [H|T]\==Z.  % the queue was nonempty

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Breadth-first-planner:

breadth_first_planner(Start,SolPath) :-
    ground(Start),
    ( goal(Start) -> SolPath = [(start->Start)]
    ; empty(InitQueue), add(InitQueue,[(start->Start)],Queue), 
      bfp(Queue,[Start],SolPath)
    ).

bfp(Queue,Visited,SolPath) :- 
    rem(Queue,[(Move->Node)|Ancestors],RemainderQueue), 
    generate_succesors(Node,SucList,Visited),
    ( member((MoveG->NodeG),SucList), goal(NodeG) ->
        reverse([(MoveG->NodeG),(Move->Node)|Ancestors],SolPath)
    ; process_children(SucList,[(Move->Node)|Ancestors],
		              RemainderQueue,ResultQueue), 
      append_nodes(SucList,Visited,NewVisited),
      bfp(ResultQueue,NewVisited,SolPath) 
    ).

generate_succesors(Node,SucList,Visited) :- 
    findall((Move->Suc),succ(Node,Move,Suc,Visited),SucList) .

succ(Node,Move,Suc,Visited) :- 
    move(Node,Move,Suc), \+ has(Visited,Suc).

%%% process_succesors([FirstSuc|Sucs],Ancestors,
%%% 		  RemainderQueue,ResultQueue) :- 
%%%     add(RemainderQueue,[FirstSuc|Ancestors],TempQueue), 
%%%     process_succesors(Sucs,Ancestors,TempQueue,ResultQueue) .
%%% process_succesors([],_Ancestors,ResultQueue,ResultQueue) .

append_nodes([],Nodes,Nodes).
append_nodes([(_Move->Node)|MNs],Nodes,[Node|NsNodes]) :-
    append_nodes(MNs,Nodes,NsNodes).

%%%%%%%%%%%%%%% The two containers problem: %%%%%%%%%%%%%%%
% There are two containers of given capacity: C1 and C2 liters of liquid.
% There is a big container, too. Its initial content is >= C1+C2.
% We have to achieve C liters of liquid in the second container.
% The possible movements:
% into(I,J): Pour the whole amount of liquid from container I into J.
% to(I,J): Fill J from I (I will not be empty).
% fill(I): Fill I from the big container.
% empty(I): Empty I into the big container.
%
% The possible movements:
% move(FromState,WithMove,ToState)
move(s(U,V),into(1,2),s(0,Y)) :- 
	U>0, c2(C2), Y is U+V, Y=<C2 .
move(s(U,V),into(2,1),s(X,0)) :- 
	V>0, c1(C1), X is U+V, X=<C1 .
move(s(U,V),to(2,1),s(C1,Y)) :- 
	c1(C1), U<C1, Y is U+V-C1, Y>0 .
move(s(U,V),to(1,2),s(X,C2)) :- 
	c2(C2), V<C2, X is U+V-C2, X>0 .
move(s(X,Y),fill(1),s(C1,Y)) :- 
	c1(C1), X<C1 .
move(s(X,Y),fill(2),s(X,C2)) :- 
	c2(C2), Y<C2 .
move(s(X,Y),empty(1),s(0,Y)) :- 
	X>0 .
move(s(X,Y),empty(2),s(X,0)) :- 
	Y>0 .


:- dynamic c1/1, c2/1, goal/1.

% The capacities of the first and second containers:
% (the capacities should be relative prime numbers)
c1(5).
c2(7).

% The goal state:
% (the amount of liquid should be an integer in the range [0..C2])
goal(s(_,4)).

init_graph(C1,C2,C) :-
	retractall(c1(_)), retractall(c2(_)),
	asserta(c1(C1)), asserta(c2(C2)),
	retractall(goal(_)),
	asserta(goal(s(_,C))).

two_containers(C1,C2,C,Solution) :-
    init_graph(C1,C2,C),
    breadth_first_planner(s(0,0),Solution).

two_container_test(C1,C2,Solution) :-
    in(C,0,C2), two_containers(C1,C2,C,Solution).

%% in(K,I,J) :- K in I..J.

in(K,I,J) :-
    integer(I), integer(J),
    ( integer(K) -> I =< K, K =< J
    ; var(K), genK(K,I,J)
    ).

genK(K,I,J) :-
    I =< J,
    ( K = I
    ; I1 is I+1, genK(K,I1,J)
    ).
