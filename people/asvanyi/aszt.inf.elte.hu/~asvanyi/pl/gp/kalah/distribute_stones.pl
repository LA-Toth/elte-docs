% This file is needed for kalah.pl .
% Its content:
% Clauses and predicates for distribute_stones/4

:- module( ditribute_stones, [distribute_stones/4] ).

:- use_module( library(lists), [nth1/3,sumlist/2] ).

distribute_stones(Stones,Hole,Board,FinalBoard) :-
    first_distribute_my_holes(Stones,Hole,Board,Board1,Stones1),
    distribute_your_holes(Stones1,Board1,Board2),
    check_if_finished(Board2,FinalBoard).

first_distribute_my_holes(Stones,Hole,
                          board(MyHoles,MyKalah,YourHoles,YourKalah),
                          board(MyHoles1,MyKalah1,YourHoles,YourKalah),
                          Stones1) :-
    Stones>=7-Hole, !,
    pick_up_and_distribute(Hole,Stones,MyHoles,MyHoles1),
    MyKalah1 is MyKalah+1,
    Stones1 is Stones+Hole-7.
first_distribute_my_holes(Stones,Hole,
                          board(MyHoles,MyKalah,YourHoles,YourKalah),
                          board(MyHoles1,MyKalah1,YourHoles1,YourKalah),0) :-
    % Stones<7-Hole, 
    FinishingHole is Hole+Stones,
    nth1(FinishingHole,MyHoles,0),
    OppositeHole is 7-FinishingHole,
    nth1(OppositeHole,YourHoles,Y),
    Y>0, !,
    Stones1 is Stones-1,
    pick_up_and_distribute(Hole,Stones1,MyHoles,MyHoles1),
    nth_substitute(OppositeHole,YourHoles,0,YourHoles1),
    MyKalah1 is MyKalah+Y+1.
first_distribute_my_holes(Stones,Hole,
                          board(MyHoles,MyKalah,YourHoles,YourKalah),
                          board(MyHoles1,MyKalah,YourHoles,YourKalah),0) :-
    /* Stones<7-Hole,
        \+ ( FinishingHole is Hole+Stones,
             nth1(FinishingHole,MyHoles,0),
             OppositeHole is 7-FinishingHole,
             nth1(OppositeHole,YourHoles,Y),
             Y>0
           ), 
       !,
    */
    pick_up_and_distribute(Hole,Stones,MyHoles,MyHoles1).

pick_up_and_distribute(H,Stones,[Hole|Holes],[Hole|Holes1]) :-
    H>1, !,
    H1 is H-1, pick_up_and_distribute(H1,Stones,Holes,Holes1).
pick_up_and_distribute(1,Stones,[_|Holes],[0|Holes1]) :-
    distribute(Stones,Holes,Holes1).
    
distribute(H,[Hole|Holes],[Hole1|Holes1]) :-
    H>0, !,
    H1 is H-1, Hole1 is Hole+1,
    distribute(H1,Holes,Holes1).
distribute(0,Holes,Holes) :- !.
distribute(_,[],[]).

nth_substitute(N,[Hole|Holes],NewHole,[Hole|NewHoles]) :-
    N>1, !,
    N1 is N-1, nth_substitute(N1,Holes,NewHole,NewHoles).
nth_substitute(1,[_|Holes],NewHole,[NewHole|Holes]).

distribute_your_holes(0,Board,Board) :- !.
distribute_your_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                      board(MyHoles,MyKalah,YourHoles1,YourKalah)) :-
    /* 1=<Stones, */ Stones=<6, !,
    distribute(Stones,YourHoles,YourHoles1).
distribute_your_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                      FinalBoard) :-
    /* Stones>6, !, */
    distribute(Stones,YourHoles,YourHoles1),
    Stones1 is Stones-6,
    distribute_remaining_stones(Stones1,
                                board(MyHoles,MyKalah,YourHoles1,YourKalah),
                                FinalBoard).

 distribute_remaining_stones(Stones,Board,FinalBoard) :-
    distribute_my_holes(Stones,Board,Board1,Stones1),
    distribute_your_holes(Stones1,Board1,FinalBoard).

distribute_my_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                    board(MyHoles1,MyKalah1,YourHoles,YourKalah), Stones1) :-
    Stones>=7, !,
    distribute(Stones,MyHoles,MyHoles1),
    MyKalah1 is MyKalah+1,
    Stones1 is Stones-7.
distribute_my_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                    board(MyHoles1,MyKalah1,YourHoles1,YourKalah), 0) :-
    /* Stones<7, */
    nth1(Stones,MyHoles,0),
    OppositeHole is 7-Stones,
    nth1(OppositeHole,YourHoles,Y),
    Y>0, !,
    Stones1 is Stones-1,
    distribute(Stones1,MyHoles,MyHoles1),
    nth_substitute(OppositeHole,YourHoles,0,YourHoles1),
    MyKalah1 is MyKalah+Y+1.
distribute_my_holes(Stones, board(MyHoles,MyKalah,YourHoles,YourKalah),
                    board(MyHoles1,MyKalah,YourHoles,YourKalah), 0) :-
    /* Stones<7,
        \+ ( nth1(Stones,MyHoles,0),
             OppositeHole is 7-Stones,
             nth1(OppositeHole,YourHoles,Y),
             Y>0
           ), 
       !,
    */
    distribute(Stones,MyHoles,MyHoles1).

check_if_finished(board(MyHoles,MyKalah,YourHoles,YourKalah),
                  board(MyHoles,MyKalah,YourHoles1,YourKalah1) ) :-
    zero(MyHoles), !,
    sumlist(YourHoles,Sum), zero(YourHoles1), YourKalah1 is YourKalah+Sum.
check_if_finished(board(MyHoles,MyKalah,YourHoles,YourKalah),
                  board(MyHoles1,MyKalah1,YourHoles,YourKalah) ) :-
    zero(YourHoles), !,
    sumlist(MyHoles,Sum), zero(MyHoles1), MyKalah1 is MyKalah+Sum.
check_if_finished(Board,Board).

zero([0,0,0,0,0,0]).

