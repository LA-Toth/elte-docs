:- module( alga, [ init_graph_search/1, graph_search/2 ] ).

init_graph_search( Module ) :-
    use_module( Module, [ arc/3, h/2, goal/1 ] ).

graph_search(Start,Solution) :- 
    alg_a([n4(Start,0,0,[])],[],Solution) -> true.

alg_a([n4(N,_,G,Path)|Open],Closed,Solution) :-
	( goal(N) -> Solution = n2(G,[N|Path])
	; findall( Nn/Gn,
		   ( arc(N,Nn,Cost), Gn is G+Cost ),
		   Sucs
		 ),
	  sucs(Sucs,[N|Path],Open,[N/G|Closed],NewOpen,NewClosed),
	  alg_a(NewOpen,NewClosed,Solution)
	).

sucs([N/G|Sucs],Path,Open,Closed,NewOpen,NewClosed) :-
    member(n4(N,F0,G0,_),Open) ->
        ( G<G0 -> F is G+F0-G0, 
            ins_del(Open,n4(N,F,G,Path),Open1),
            sucs(Sucs,Path,Open1,Closed,NewOpen,NewClosed)
          ; sucs(Sucs,Path,Open,Closed,NewOpen,NewClosed)
	)
    ; member(N/G0,Closed) ->
        ( G<G0 -> h(N,H), F is G+H,
            del1(Closed,N/_,Closed1), ins(Open,n4(N,F,G,Path),Open1),
            sucs(Sucs,Path,Open1,Closed1,NewOpen,NewClosed)
          ; sucs(Sucs,Path,Open,Closed,NewOpen,NewClosed)
	)
    ; h(N,H), F is G+H, ins(Open,n4(N,F,G,Path),Open1),
      sucs(Sucs,Path,Open1,Closed,NewOpen,NewClosed).
sucs([],_,Open,Closed,Open,Closed) .

del1([Y|LX],X,[Y|L]) :- X\=Y, !, del1(LX,X,L) .
del1([X|L],X,L).

% ins([Y|L],X,[Y|LX]) :- gt(X,Y), !, ins(L,X,LX) . % red cut !!!
% ins(L,X,[X|L]) .

ins([],X,[X]) .
ins([Y|Ys],X,Xs) :-
	( gt(X,Y) -> Xs = [Y|Zs], ins(Ys,X,Zs)
	; Xs = [X,Y|Ys]
	).

ins_del([Z|LY],X,LZX) :- gt(X,Z), !, LZX=[Z|LX], ins_del(LY,X,LX). % red cut !!!
ins_del(LY,X,[X|L]) :- node_to_be_deleted(X,Y), del1(LY,Y,L).

node_to_be_deleted(n4(N,_,_,_),n4(N,_,_,_)) .

gt(n4(_,F1,_,_),n4(_,F2,_,_)) :- F1>F2 .
