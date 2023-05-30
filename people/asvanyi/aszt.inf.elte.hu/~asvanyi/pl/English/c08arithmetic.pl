%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Chapter 8: Basic Use of Arithmetic in Prolog               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   greatest_common_divisor(X,Y,Z) :- 
	Z is the greatest common divisor of the integers X and Y.
*/
     greatest_common_divisor(J,I,Gcd) :-
          J > 0, R is I rem J, greatest_common_divisor(R,J,Gcd).
     greatest_common_divisor(0,I,I).

%  Program 8.1    Computing the greatest common divisor of two integers

gcd(J,I,Gcd) :-
    ( J > 0 -> R is I rem J, gcd(R,J,Gcd)
    ; Gcd = I
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
   factorial(N,F) :- F is the integer N factorial.
*/ 
     factorial(N,F) :-
        N > 0, N1 is N-1, factorial(N1,F1), F is N*F1.
     factorial(0,1).
%
%  Program 8.2   Computing the factorial of a number

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   factorial2(N,F) :- F is the integer N factorial.
*/ 
     factorial2(N,F) :- factorial(0,N,1,F).

     factorial(I,N,T,F) :-       % F is T*(I+1)*(I+2)*...*N if I=<N
        I < N, I1 is I+1, T1 is T*I1, factorial(I1,N,T1,F).
     factorial(N,N,F,F). 

%  Program 8.3   An iterative factorial

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   factorial3(N,F) :- F is the integer N factorial.
*/ 
     factorial3(N,F) :- factorial(N,1,F). 

     factorial(N,T,F) :-                % F is T*(N!) if N>=0
        N > 0, T1 is T*N, N1 is N-1, factorial(N1,T1,F).
     factorial(0,F,F). 

%  Program 8.4   Another iterative factorial

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  
  between(+I,+J,?K) :- K is an integer between the integers I and J inclusive.
*/
     :- mode between(+,+,?).

     between(I,J,I) :- I =< J.
     between(I,J,K) :- I < J, I1 is I + 1, between(I1,J,K).

%  Program 8.5   Generating a range of integers

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   sumlist(Is,Sum) :- Sum is the sum of the list of integers Is.
*/
     sumlist([I|Is],Sum) :- sumlist(Is,IsSum), Sum is I+IsSum.
     sumlist([],0).

%  Program 8.6a   Summing a list of integers

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   sumlist2(Is,Sum) :- Sum is the sum of the list of integers Is.
*/
     sumlist2(Is,Sum) :- sumlist(Is,0,Sum).

% sumlist(List,Temp,Sum) :- Sum is Temp + sum(L).

     sumlist([I|Is],Temp,Sum) :-    
	Temp1 is Temp+I, sumlist(Is,Temp1,Sum).
     sumlist([],Sum,Sum).

%  Program 8.6b   Iterative version of summing a list of integers
%					using an accumulator

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   inner_product(Xs,Ys,Value) :- 
	Value is the inner product of the vectors
	represented by the lists of integers Xs and Ys.
*/
     inner_product([X|Xs],[Y|Ys],IP) :-
	inner_product(Xs,Ys,IP1), IP is X*Y + IP1.
     inner_product([],[],0).

%  Program 8.7a   Computing inner products of vectors

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   inner_product(Xs,Ys,Value) :- 
	Value is the inner product of the vectors
	represented by the lists of integers Xs and Ys.
*/
     inner_product2(Xs,Ys,IP) :- inner_product(Xs,Ys,0,IP).
 
     inner_product([X|Xs],[Y|Ys],Temp,IP) :-
         Temp1 is X*Y+Temp, inner_product(Xs,Ys,Temp1,IP).
     inner_product([],[],IP,IP).

%  Program 8.7b   Computing inner products of vectors iteratively

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   area(Chain,Area) :- 
	Area is the area of the polygon enclosed by the list of points
	Chain, where the coordinates of each point are represented by 
	a pair (X,Y) of integers.
*/
     area([_],0).
     area([(X1,Y1),(X2,Y2)|XYs],Area) :-
        area([(X2,Y2)|XYs],Area1), 
        Area is (X1*Y2-Y1*X2)/2 + Area1.

%  Program 8.8  Computing the area of polygons

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
   maxlist(Xs,N) :- N is the maximum of the list of integers Xs.
*/
     maxlist([X|Xs],M) :- maxlist(Xs,X,M).

     % maxlist(Xs,M,N) :- N is the maximum of the list of integers [M|Xs]
     maxlist([X|Xs],Y,M) :- maximum(X,Y,Y1), maxlist(Xs,Y1,M).
     maxlist([],M,M).

	maximum(X,Y,Y) :- X =< Y.
	maximum(X,Y,X) :- X > Y.

%  Program 8.9    Finding the maximum of a list of integers

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   length_check(Xs,N) :- Xs is a list of length N.
*/
     length_check([_X|Xs],N) :- N > 0, N1 is N-1, length_check(Xs,N1).
     length_check([],0).

%  Program 8.10   Checking the length of a list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   length1(Xs,N) :- N is the length of the list Xs.
*/
     length1([_|Xs],N) :- length1(Xs,N1), N is N1+1.
     length1([],0).

%  Program 8.11  Finding the length of a list

length2(Xs,N) :- length2(Xs,0,N).

length2([_X|Xs],A,N) :-  A1 is A+1, length2(Xs,A1,N).
length2([],N,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
   range(M,N,Ns) :- Ns is the list of integers between M and N inclusive.
*/
     range(M,N,[M|Ns]) :- M < N, M1 is M+1, range(M1,N,Ns).
     range(N,N,[N]).

%  Program 8.12   Generating a list of integers in a given range

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range1(M,N,Xs) :- range1(M,N,[],Xs).

range1(M,N,As,Xs) :- M<N , N1 is N-1, range1(M,N1,[N|As],Xs).
range1(M,M,As,[M|As]).
