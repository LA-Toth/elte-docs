:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

breadth_first_search(Start,Goal,SolPath) :-
    var(SolPath),         % We suppose that the solution is still unknown.
    empty(InitQueue), add(InitQueue,[Start],Queue), 
    bfs(Queue,[Start],Goal,SolPath).

bfs(Queue,Visited,Goal,SolPath) :- 
    rem(Queue,[Node|Ancestors],RemainderQueue), 
    succesors(Node,SucList,Visited), 
    process_sucs(SucList,[Node|Ancestors],Goal,RemainderQueue,
                 ResultQueue,SolPath), 
    ( nonvar(SolPath) -> true 
    ; append(SucList,Visited,NewVisited),
      bfs(ResultQueue,NewVisited,Goal,SolPath) 
    ) .

succesors(Node,SucList,Visited) :- 
    findall(Suc,succ(Node,Suc,Visited),SucList) .

succ(Node,Suc,Visited) :- 
    edge(Node,Suc), \+ member(Suc,Visited).

process_sucs([Goal|_],Ancestors,Goal,_,_,SolPath) :-
    !, 
    reverse([Goal|Ancestors],SolPath). 
process_sucs([FirstSuc|Sucs],Ancestors,Goal,RemainderQueue,
	     ResultQueue,SolPath) :- 
    !, 
    add(RemainderQueue,[FirstSuc|Ancestors],TempQueue), 
    process_sucs(Sucs,Ancestors,Goal,TempQueue,ResultQueue,SolPath) .
process_sucs([],_Ancestors,_Goal,ResultQueue,ResultQueue,_SolPath) .

:- use_module(library(lists),[reverse/2]).

edge([X,-|Xs],[-,X|Xs]).
edge([-,X|Xs],[X,-|Xs]).
edge([-,b,w|Xs],[w,b,-|Xs]).
edge([b,w,-|Xs],[-,w,b|Xs]).
edge([X|Xs],[X|Ys]) :- X \== -, edge(Xs,Ys).


% Queue-handling with difference-pair lists .

%empty(Queue): Queue is an empty list (d-list of the form Var-Var)
empty(Q-Q) :- var(Q).

% add(+InQ,?ITEM,-ResQ): ITEM put at the end of d-list Inq 
%    we receive d-list (i.e. Queue) Resq  of the form PartialList-Variable.
% preCond: InQ is a d-list of the form PartialList-Variable.
add(Q1-[ITEM|Y],ITEM,Q1-Y).

% ?- add([P1,P2,...,PN|Z]-Z,ITEM,R).

% add(Q1-X,ITEM,Q2) :- X = [ITEM|Y], Q2 = Q1-Y.

rem([H|T]-Z,H,T-Z) :- 
     [H|T]\==Z.

