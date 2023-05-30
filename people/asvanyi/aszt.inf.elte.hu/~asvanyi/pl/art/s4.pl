%% -*- Mode: Prolog; coding: utf-8 -*- 


%% http://aszt.inf.elte.hu/~asvanyi/pl/art/s4.pl

%% http://www.sics.se/isl/sicstus/license4-academic.html
%%                                             (EXHIBIT D)

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(100)]).

:- set_prolog_flag(legacy_char_classification,on).

%% legacy_char_classification
%%      'on' or 'off'.  When enabled, most legal Unicode codepoints above
%%      255 are treated as lowercase characters when reading Prolog terms.

%% Azaz, ha prolog_flag(legacy_char_classification,on)
%% => utf-8 kódolás mellett a magyar kis- és nagybetűket általában jól
%% kezeli és különbözteti meg a SICStus;
%% KIVÉTEL: az Ő és Ű kisbetűnek számít. 

apja('Ábrahám','Izsák').     apja('Ábrahám','Izmael').
apja('Ábrahám','Ismeretlen').
apja('Izsák','Jákób').       apja('Izsák','Ézsau').
apja('Jákób','József').      apja('Jákób','Benjámin').

anyja('Sára','Izsák').       anyja('Hágár','Izmael').
anyja('Rebeka','Jákób').     anyja('Rebeka','Ézsau').

férfi('Ábrahám').  férfi('Izsák').  férfi('Izmael').
férfi('Jákób').    férfi('Ézsau').
férfi('József').   férfi('Benjámin').

nő('Sára').        nő('Hágár').     nő('Rebeka').
nő('Ismeretlen').

szereti(_Bárki,'Sára').

eq(X,X).

'Ábrahám lánya'(X) :- apja('Ábrahám',X), nő(X).

apa(Valaki) :- apja(Valaki,_Gyermek).

apa_és_fiú(Valaki) :- apja(Valaki,_), szülője(_,Valaki).

szülője(X,Y) :- anyja(X,Y).
szülője(X,Y) :- apja(X,Y).

fia(X,Y) :- szülője(Y,X), férfi(X).

lánya(X,Y) :- szülője(Y,X), nő(X).

nagyszülője(X,Y) :-
    szülője(X,Z),
    szülője(Z,Y).

őse(X,Y) :- szülője(X,Y).
őse(X,Y) :- szülője(X,Z), őse(Z,Y).

őse_(X,Y) :- szülője(X,Y). 
őse_(X,Y) :- őse_(Z,Y), szülője(X,Z). %Végtelen ág.

őse00(X,Y) :- szülője(X,Y).
őse00(X,Y) :- őse00(X,Z), őse00(Z,Y). %Végtelen ág.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A természetes számok s-szám reprezentációja:
%%                        0, s(0), s(s(0)), ...
nat(0).
nat(s(N)) :- nat(N).

%% Előf: X vagy Z valódi s-szám.
%% plus(X,Y,Z) :- X és Y s-számok összege a Z s-szám.
plus(0,N,N).
plus(s(M),N,s(K)) :- plus(M,N,K).  % (M+1)+N = (M+N)+1

%% Előf: X valódi s-szám, továbbá Y vagy Z is az.
%% times(X,Y,Z) :- X és Y s-számok szorzata a Z s-szám.
times(0,_N,0).
times(s(M),N,K) :- plus(N,MN,K), times(M,N,MN).  % (M+1)*N = M*N+N

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list([]).
list([_X|Xs]) :- list(Xs).
%% list(.(X,Xs)) :- list(Xs).

%% .(X,Xs) == [X|Xs]
%% [X|[Y|Zs]] == [X,Y|Zs]
%% [X1,X2,...,Xn|[]] == [X1,X2,...,Xn]

%% Pl.: Listák: [], .(1,[]), .(1,.(2,[])), ...
%% .(1,.(2,.(3,[]))) == [1|[2|[3|[]]]] == [1,2|[3|[]]] == [1,2,3|[]]
%% .(1,.(2,.(3,[]))) == [1|[2,3|[]]] == [1|[2|[3]]] == [1,2|[3]]
%% .(1,.(2,.(3,[]))) == [1,2,3] == [1|[2,3]]

%% Előf: Xs valódi lista.
%% member_(X,Xs): X eleme az Xs listának.
%% member_(X,Xs) :- member(X,Xs).
member_(X,[X|_Xs]).
member_(X,[_X|Xs]) :- member_(X,Xs).

%% Előf: Ys valódi (nemüres) lista.
%% select(X,Ys,Zs) :- X listaelemet kiválasztva és eltávolítva 
%%                    Ys-ből a Zs valódi lista marad.

select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Zs]) :- select(X,Xs,Zs).


/*
| ?- select(X,[a,b,c,d],Ms).
X = a,
Ms = [b,c,d] ? ;
X = b,
Ms = [a,c,d] ? ;
X = c,
Ms = [a,b,d] ? ;
X = d,
Ms = [a,b,c] ? ;
no
*/

%% Előf: Xs valódi lista.
%% perm(Xs,Ys) :- Xs permutáltja Ys.
/*
| ?- perm([],Ps).
Ps = []
| ?- perm([5],Ps).
Ps = [5]
| ?- perm([A,B],Ps).
Ps = [A,B] ? ;    Ps = [B,A] ? ;
no
| ?- perm([a,b,c],Ps).
Ps = [a,b,c] ? ;    Ps = [a,c,b] ? ;
Ps = [b,a,c] ? ;    Ps = [b,c,a] ? ;
Ps = [c,a,b] ? ;    Ps = [c,b,a] ? ;
no
*/

%%% perm([],[]).
%%% perm([X|Xs],Zs) :- perm(Xs,Ys), ins(Ys,X,Zs).

perm([],[]).
%% perm([X|Xs],[Y|Zs]) :- select(Y,[X|Xs],Ys), perm(Ys,Zs).
perm(Xs,[Y|Zs]) :- select(Y,Xs,Ys), perm(Ys,Zs).

%% Előf: Xs valódi lista.
%% ins(Xs,Y,Zs) :-  Xs-be nemdeterminisztikusan beszúrva az Y elemet,
%%                  a Zs valódi lista adódik.

ins(Xs,X,Ys) :- select(X,Ys,Xs).

%% Előf: Xs vagy XsYs valódi lista.
%% append_(Xs,Ys,XsYs):-
%%     Az Xs és Ys listák konkatenáltja az XsYs lista.
%% append_(Xs,Ys,XsYs) :- append(Xs,Ys,XsYs).
append_([],Ys,Ys).
append_([X|Xs],Ys,[X|Zs]) :- append_(Xs,Ys,Zs). 

%% Előf: Xs valódi lista.
%% naive_reverse(Xs,Ys): Az Xs lista fordítottja az Ys lista.
%%                       (Naiv megoldás.)
naive_reverse([],[]).
naive_reverse([X|Xs],Zs) :-
    naive_reverse(Xs,Ys), append_(Ys,[X],Zs).

%% LI(N) = N*(N+1)/2 + N+1 eleme Theta(N^2) ahol N a lista hossza.
%% LI : Logical Inference

%% Előf: Xs valódi lista.
%% rev_app(Xs,Ys,Zs): 
%%     Az Xs lista fordítottját az Ys listával konkatenálva
%%     kapjuk a Zs listát.

rev_app([],Ys,Ys).
rev_app([X|Xs],Ys,Zs) :- rev_app(Xs,[X|Ys],Zs).

%% Előf: Xs valódi lista.
%% reverse(Xs,Ys): Az Xs lista fordítottja az Ys lista.
reverse(Xs,Ys) :- rev_app(Xs,[],Ys).
%% LI(N) = N+2 eleme Theta(N) ahol N a lista hossza.

%% Előf: Xs valódi lista.
%% div2(Xs,P1s,P2s):
%%     Xs páratlan sorszámú elemi P1s-ben, a páros sorszámúak P2s-ben,
%%     az Xs-beli sorrendben.

div2([],[],[]).
div2([X],[X],[]).
div2([X,Y|Xs],[X|Zs],[Y|Ys]) :- div2(Xs,Zs,Ys).

div2_([X|Xs],[X|Ys],Zs) :- div2_(Xs,Zs,Ys).
div2_([],[],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ?- bintree(t(t(o,1,t(o,2,o)),3,t(t(o,4,o),5,o))).

bintree(o).
bintree(t(Left,_Key,Right)) :- bintree(Left), bintree(Right).

%% Előf: Tree valódi (proper) bináris fa.
%% inorder(Tree,Is) :- A Tree bináris fa inorder bejárása Is.

%% | ?- inorder(t(t(t(o,1,t(o,2,o)),3,t(o,4,o)),5,t(o,6,t(o,7,o))),Xs).
%% Xs = [1,2,3,4,5,6,7]

inorder(o,[]).
inorder(t(L,K,R),Is) :- inorder(L,Ls), inorder(R,Rs), append(Ls,[K|Rs],Is).

inorderOpt(T,Is) :- inorder_app(T,[],Is).

inorder_app(o,Is,Is).
inorder_app(t(L,K,R),Ys,Is) :- inorder_app(R,Ys,Zs), inorder_app(L,[K|Zs],Is).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%                         Aritmetika
%                         ----------

% | ?- X is -2**3, Y is floor(cos(0))+3*(8-7//2).
% X = -8.0,    Y = 16
% | ?- X = -2**3, Y = floor(cos(0))+3*(8-7//2).
% X = -2**3,    Y = floor(cos(0))+3*(8-7//2)
% | ?- 5 is 2+3, 5.0 =:= 2+3.
% yes
% | ?- 2+3 is 2+3.
% no
% | ?- 5.0 is 2+3.
% no
% | ?- Y is 6+1.
% Y = 7 
% | ?- Y =:= 6+1.
% ! Instantiation error in argument 1 of (=:=)/2
% ! goal:  _419=:=6+1

% Aritmetikai összehasonlító predikátumok:
% K1 =:= K2, K1 =\= K2, K1 > K2, K1 < K2, K1 >= K2, K1 =< K2

%% fact(N,F) :- F az N természetes szám faktoriálisa.
/*
| ?- fact(0,F).      F = 1 ? ;      no
| ?- fact(1,F).      F = 1 ? ;      no
| ?- fact(2,F).      F = 2 ? ;      no
| ?- fact(3,F).      F = 6 ? ;      no
| ?- fact(4,F).      F = 24 ? ;     no
| ?- fact(5,F).      F = 120 ? ;    no
*/

fact(N,F) :-
    N > 0,
    K is N-1,
    fact(K,L),
    F is N*L.
fact(0,1).

fac(N,F) :- fac_(N,1,F).

fac_(N,A,F) :-
    N > 0,
    N1 is N-1,
    A1 is N*A,
    fac_(N1,A1,F).
fac_(0,A,A).

% f(0)=1. f(1)=1. f(N)=f(N-1)+f(N-2) :- N>1.
fib0(0,1).
fib0(1,1).
fib0(N,F) :-
    N>1,
    N1 is N-1, N2 is N-2,
    fib0(N1,F1), fib0(N2,F2),
    F is F1+F2.


fib1(N,F) :- N>0, fib_(N,1,1,1,F).
fib1(0,1).

fib_(N,N,FN,_FN_1,FN).
fib_(N,K,FK,FK_1,FN) :- 
    N>K,
    K1 is K+1, FK1 is FK+FK_1,
    fib_(N,K1,FK1,FK,FN).

fib2(N,F) :- N>0, N_1 is N-1, fib2_(N_1,1,1,F).
fib2(0,1).

fib2_(N_K,FK,FK_1,FN) :- 
    N_K>0,
    N_K1 is N_K-1, FK1 is FK+FK_1,
    fib2_(N_K1,FK1,FK,FN).
fib2_(0,FN,_FN_1,FN).

fib3(N,F) :- N>0, fib3_(N,1,1,F).
fib3(0,1).

fib3_(N1_K,FK,FK_1,FN) :- % N>=K
    N1_K>1,
    N_K is N1_K-1, FK1 is FK+FK_1,
    fib3_(N_K,FK1,FK,FN).
fib3_(1,FN,_FK1,FN).


fs0(N,Fs) :-
    N>1,
    N1 is N-1, fs0(N1,F1s),
    fib1(N,F), append(F1s,[F],Fs).
fs0(1,Fs) :- fib1(1,F), Fs =[F].

fs3(N,Fs) :- fs3app(N,[],Fs).

fs3app(N,Xs,Fs) :-
    N>0,
    fib3(N,FN),
    N1 is N-1,
    fs3app(N1,[FN|Xs],Fs).
fs3app(0,Xs,[F1|Xs]) :- fib3(0,F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% addv(V1,V2,V) :-
%     A V vektor a V1 és V2 vektorok összege. 
addv([],[],[]).
addv([X|Xs],[Y|Ys],[Z|Zs]) :- 
    Z is X+Y, addv(Xs,Ys,Zs).
    
% mult(V1,V2,S) :-
%     S a V1 és V2 vektorok skaláris szorzata.
mult_([],[],0).
mult_([X|Xs],[Y|Ys],S) :-
    mult_(Xs,Ys,S0), S is S0+X*Y.

mult(V1,V2,S) :- mult(V1,V2,0,S).

% mult(V1,V2,A,S) iff S = A+V1*V2.
mult([],[],A,A).
mult([X|Xs],[Y|Ys],A0,S) :-
    A1 is A0+X*Y, mult(Xs,Ys,A1,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STO és NSTO programok %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Előf: Xs valódi lista.
%% member_(X,Xs):- X eleme az Xs listának (NSTO célokhoz).

%%% member_(X,[X|_Xs]).
%%% member_(X,[_X|Xs]) :- member_(X,Xs).

%% eleme(X,Xs): X eleme az Xs listának (STO változat).
eleme(X,[Z|_Xs]) :- unify_with_occurs_check(X,Z).
eleme(X,[_X|Xs]) :- eleme(X,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%                 Termek típusa és összehasonlítása
%                 ---------------------------------

% | ?- var(X), var(Y), var(Z), var(U), X=1, Y=1.0, Z=a, U=f(a), 
%      nonvar(X), nonvar(Y), nonvar(Z), nonvar(U), compound(U),
%      atomic(X), atomic(Y), atomic(Z), atom(Z),
%      number(X), number(Y), integer(X), float(Y).
%
% U = f(a),    X = 1,    Y = 1.0,    Z = a

% | ?- X \== Y, X=Y, X==Y.
% Y = X

% | ?- X @< 0.0, 0.0 @< 0, 0 @< a, a @< f(z), f(z) @< f(a,a).
% true
% | ?- 5 < 5.1, 5.1 @< 5, 9.9e99 @< -999999999.
% yes

%% Aritmetikai öh  :   =:=   =\=   >   <   >=   =<
%% St. rend. sz. öh:   ==    \==  @>  @<  @>=  @=<

%% Egyenlőségek    :    ==    =   is   =:=   =..
%% Nem-egyenlőségek:   \==   \=        =\=

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A vágó utasítás: A felesleges programágak levágása %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% minimum(@X,@Y,?Min): Min az X és az Y számok közül a kisebbik.
% Vigyázat: (2.0\=2)
% | ?- minimum(2.0,3,2).
% no

% X=<Y esetén felesleges választási pontot hagy:
minimum0(X,Y,X) :- X =< Y.	
minimum0(X,Y,Y) :- X > Y.

% Zöld vágás: A szemantikát nem érinti.
minimum1(X,Y,X) :- X =< Y, !.    % green cut
minimum1(X,Y,Y) :- X > Y.

% | ?- minimum1(2,3,2).
% yes
% | ?- minimum1(2,3,3).
% no
% | ?- minimum1(2,3,X).
% X = 2

% Piros vágás: Korai meghiúsulást okozhat. Hibás!
minimum2(X,Y,X) :- X =< Y, !.    % red cut
minimum2(_X,Y,Y).

% | ?- minimum2(2,3,2).
% yes
% | ?- minimum2(2,3,3).
% yes
% | ?- minimum2(2,3,X).
% X = 2

% Helyesen: az output illesztés csak a vágó után:
minimum3(X,Y,Z) :- X =< Y, !, Z = X.  % red cut
minimum3(_,Y,Y).

% | ?- minimum3(2,3,2).
% yes
% | ?- minimum3(2,3,3).
% no
% | ?- minimum3(2,3,X).
% X = 2

minimum4(X,Y,Z) :- X =< Y, !, Z = X.    % green cut : opt
minimum4(X,Y,Y) :- X > Y.

% Javasolt megoldás: (If->Then;Else)
% Rászorít bennünket, hogy elég általánosan
% adjuk meg az output formális paramétert,
% és hatékonyabb is.

minimum(X,Y,Z) :-
    ( X =< Y -> Z = X
    ; Z = Y
    ). 

% | ?- minimum(2,3,2).
% yes
% | ?- minimum(2,3,3).
% no
% | ?- minimum(2,3,X).
% X = 2

minimum_rossz_0(X,Y,Z) :-
    ( X =< Y, Z = X
    ; Z = Y
    ). 

minimum_rossz(X,Y,Z) :-
    ( X =< Y, Z = X -> true
    ; Z = Y
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nagyszülője_(X,Y) :- 
    ( var(X), nonvar(Y) -> szülője(Z,Y), szülője(X,Z)
    ; szülője(X,Z), szülője(Z,Y)
    ).

%%% ( a1, a2 -> b1, b2
%%% ; c -> d
%%% ; e -> f
%%% ; g
%%% )

%%% ( a -> b ) egyenértékű azzal, hogy ( a -> b ; fail )
%%% ( a -> b ; c ) egyenértékű azzal, hogy ( ( a -> b ) ; c )

%%% ( valami -> true ), true ; akármi %% egyenértékű azzal, hogy
%%% ( valami -> true ; fail ) ; akármi

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rendbeszúr(@Ys,@X,?Zs):
%   Ys (szabványos term rendezés szerint) 
%   monoton növekvő valódi Prolog listába 
%   X rendezett beszúrásával adódik Zs.
% Előf: Ys valódi lista. Zs változóidegen (X,Ys) -től.
%   Zs változó, vagy változóduplikációt nem tartalmazó lista.

% Zöld vágóval (with green cut):
rendbeszúr1([],X,[X]).
rendbeszúr1([Y|Ys],X,[X,Y|Ys]) :-
    X @=< Y, !.
rendbeszúr1([Y|Ys],X,[Y|Us]) :-
    X @> Y, rendbeszúr1(Ys,X,Us).

% Zöld vágóval, output illesztés a vágás után:
rendbeszúr2([],X,[X]).
rendbeszúr2([Y|Ys],X,Zs) :-
    X @=< Y, !, Zs = [X,Y|Ys].
rendbeszúr2([Y|Ys],X,[Y|Us]) :-
    X @> Y, rendbeszúr2(Ys,X,Us).

% Piros vágóval (with red cut),
% output illesztés a vágás után (itt kritikus):
rendbeszúr3([],X,[X]).
rendbeszúr3([Y|Ys],X,Zs) :-
    X @=< Y, !, Zs = [X,Y|Ys].
rendbeszúr3([Y|Ys],X,[Y|Us]) :-
    rendbeszúr3(Ys,X,Us).

% Javasolt mo ((If->Then;Else)-vel):
rendbeszúr([],X,[X]).
rendbeszúr([Y|Ys],X,Zs) :- 
    ( X @=< Y -> Zs = [X,Y|Ys]
    ; Zs = [Y|Us], rendbeszúr(Ys,X,Us)
    ).

% A teszteléshez:

beszúró_rendezés([],[]).
beszúró_rendezés([X|Xs],Ys) :-
    beszúró_rendezés(Xs,Zs), rendbeszúr(Zs,X,Ys).

% Végrekurzív változat:
beszúrva_rendez(Xs,Ys) :- beszúrva_rendez_(Xs,[],Ys).

%% beszúrva_rendez_(Xs,As,Ys) :-
%%     Ys úgy adódik, hogy az As rend.listába rend. besz. Xs elemeit.
beszúrva_rendez_([],Ys,Ys).
beszúrva_rendez_([X|Xs],As,Ys) :-
     rendbeszúr(As,X,Bs), beszúrva_rendez_(Xs,Bs,Ys).

% A zöld vágós változathoz:
beszúrva_rendez_1(Xs,Ys) :- beszúrva_rendez_1_(Xs,[],Ys).

beszúrva_rendez_1_([],Ys,Ys).
beszúrva_rendez_1_([X|Xs],As,Ys) :-
     rendbeszúr1(As,X,Bs), beszúrva_rendez_1_(Xs,Bs,Ys).

% A 2. zöld vágós változathoz:
beszúrva_rendez_2(Xs,Ys) :- beszúrva_rendez_2_(Xs,[],Ys).

beszúrva_rendez_2_([],Ys,Ys).
beszúrva_rendez_2_([X|Xs],As,Ys) :-
     rendbeszúr2(As,X,Bs), beszúrva_rendez_2_(Xs,Bs,Ys).

% A piros vágós változathoz:
beszúrva_rendez_3(Xs,Ys) :- beszúrva_rendez_3_(Xs,[],Ys).

beszúrva_rendez_3_([],Ys,Ys).
beszúrva_rendez_3_([X|Xs],As,Ys) :-
     rendbeszúr3(As,X,Bs), beszúrva_rendez_3_(Xs,Bs,Ys).

:- use_module(library(random),[random/3]).

randseq(Ennyit,EgytölEddig,RandSeq) :-
    Korlát is EgytölEddig + 1, randseq_(Ennyit,Korlát,RandSeq).

randseq_(Ennyit,Korlát,RandSeq) :-
    ( Ennyit > 0 -> random(1,Korlát,R), Ennyit1 is Ennyit - 1,
                    RandSeq = [R|Rs], randseq_(Ennyit1,Korlát,Rs)
    ; RandSeq = []
    ).

% Teszt:
%   (ms/2 méri, hogy egy cél hány millisecond processzoridőt
%    igényel.
ms(Goal,MilliSeconds) :-
    statistics(runtime,_), Goal, statistics(runtime,[_,MilliSeconds]).
%
 %%% | ?- randseq(1000,10000,_L),
 %%%      ms(beszúrva_rendez_1(_L,_L1),M1),
 %%%      ms(beszúrva_rendez_2(_L,_L2),M2),
 %%%      ms(beszúrva_rendez_3(_L,_L3),M3),
 %%%      ms(beszúrva_rendez(_L,_L4),M4).
% 
% M1 = 110,
% M2 = 48,
% M3 = 47,
% M4 = 15
%
%
% Fordítva:
 %%% | ?- randseq(1000,10000,_L),
 %%%      ms(beszúrva_rendez(_L,_L4),M4),
 %%%     ms(beszúrva_rendez_3(_L,_L3),M3),
 %%%      ms(beszúrva_rendez_2(_L,_L2),M2),
 %%%      ms(beszúrva_rendez_1(_L,_L1),M1).
% 
% M1 = 46,
% M2 = 46,
% M3 = 94,
% M4 = 31

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% noteq(X,Y): X nem illeszthető Y-nal (ISO Prolog: X\=Y)
noteq(X,Y) :- ( X=Y -> fail ; true ).

%% true.
%% fail :- 1==2.

% nincs_benne(Xs,Y): Az Xs listán nem található Y. 
nincs_benne([],_Y).
nincs_benne([X|Xs],Y) :-
    ( X = Y -> fail
    ; nincs_benne(Xs,Y)
    ).

% non_member_(X,Xs): nincs_benne(Xs,X).
% non_member_(X,Xs) :- nonmember(X,Xs).
non_member_(X,Xs) :- ( member_(X,Xs) -> fail ; true ).

% unió(Xs,Ys,Zs):
%      Az Xs lista Ys listán nem szereplő elemeit
%      sorban Ys elé kapcsolva kapjuk a Zs listát.
unió([],Ys,Ys).
unió([X|Xs],Ys,Zs) :-
    ( member_(X,Ys) -> unió(Xs,Ys,Zs)
    ; Zs = [X|Us], unió(Xs,Ys,Us)
    ).

unio([],Ys,Ys).
unio([X|Xs],Ys,Zs) :- member_(X,Ys), unio(Xs,Ys,Zs).
unio([X|Xs],Ys,Zs) :- non_member_(X,Ys), Zs = [X|Us],unio(Xs,Ys,Us).

unió1([],Ys,Ys).
unió1([X|Xs],Ys,Zs) :-
    member_(X,Ys), !, unió1(Xs,Ys,Zs). %zöld vágó
unió1([X|Xs],Ys,[X|Us]) :-
    \+ member_(X,Ys), unió1(Xs,Ys,Us).

unió2([],Ys,Ys).
unió2([X|Xs],Ys,Zs) :-
    member_(X,Ys), !, unió2(Xs,Ys,Zs). %piros vágó
unió2([X|Xs],Ys,[X|Us]) :-
    unió2(Xs,Ys,Us).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% union(Xs,Ys,Zs) :-
%%     Zs az Xs és Ys szig.mon.növ. valódi listák rend. uniója.

union([],Ys,Ys).
union([X|Xs],[],Zs) :- !, Zs = [X|Xs].
union([X|Xs],[Y|Ys],Zs) :-
    ( X @< Y -> Zs = [X|Us], union(Xs,[Y|Ys],Us)
    ; X @> Y -> Zs = [Y|Us], union([X|Xs],Ys,Us) 
    ; Zs = [Y|Us], union(Xs,Ys,Us)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% metszet(Xs,Ys,Zs): 
%     Az Xs lista Ys listán nem szereplő 
%     elemei elhagyásával adódik a Zs lista.
metszet([],_Ys,[]).
metszet([X|Xs],Ys,Zs) :-
    ( member_(X,Ys) ->
          Zs = [X|Ms], metszet(Xs,Ys,Ms)
    ; metszet(Xs,Ys,Zs)
    ).

metszet0([],_Ys,[]).
metszet0([X|Xs],Ys,[X|Ms]) :-
    member_(X,Ys), metszet0(Xs,Ys,Ms).
metszet0([X|Xs],Ys,Zs) :-
    \+ member_(X,Ys), metszet0(Xs,Ys,Zs).

metszet1([],_Ys,[]).
metszet1([X|Xs],Ys,[X|Ms]) :-
    member_(X,Ys), !, metszet1(Xs,Ys,Ms).
metszet1([X|Xs],Ys,Zs) :-
    \+ member_(X,Ys), metszet1(Xs,Ys,Zs).

% Hibás piros vágó:     % metszet1_([1,2,3],[1,2],[2]).
metszet1_([],_Ys,[]).
metszet1_([X|Xs],Ys,[X|Ms]) :-
    member_(X,Ys), !, metszet1_(Xs,Ys,Ms).
metszet1_([_X|Xs],Ys,Zs) :-
    metszet1_(Xs,Ys,Zs).

metszet2([],_Ys,[]).
metszet2([X|Xs],Ys,Zs) :-
    member_(X,Ys), !, Zs=[X|Ms], metszet2(Xs,Ys,Ms).
metszet2([_X|Xs],Ys,Zs) :-
    metszet2(Xs,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% member1(X,Xs):
%    X eleme Xs listának az első megoldásra korlátozva.
member1(X,Xs) :- ( member_(X,Xs) -> true ).

% member_1(X,Xs): X eleme Xs listának, az 1. mo-ra korlátozva.
member_1(X,[X|_Xs]) :- !. % piros vágó
member_1(X,[_X|Xs]) :- member_1(X,Xs).

% member_check(X,Xs):
%    X eleme Xs listának az első megoldásra korlátozva.
% member_check(_X,[]) :- fail.    % redundáns szabály
member_check(X,[Y|Ys]) :-
    ( X = Y -> true
    ; member_check(X,Ys)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% A negáció %%%

nőtlen_hallgató_(X) :- hallgató(X), nőtlen(X).

nőtlen_hallgató_f(X) :- nőtlen(X), hallgató(X).

nőtlen(X) :- ( nős(X) -> fail ; true ).

hallgató('Péter'). hallgató('János'). hallgató('Jakab').
nős('Péter').      nős('József').

nem(P) :- ( P -> fail ; true ).

%% :-(nem(P),;(->(P,fail),true)).

nőtlen_hallgató1(X) :- hallgató(X), nem(nős(X)).

nőtlen_hallgató2(X) :- nem(nős(X)), hallgató(X).

nőtlen_hallgató(X) :- hallgató(X),  \+nős(X).

nőtlen_hallgató2_(X) :-  \+nős(X), hallgató(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ismeretlen struktúrák kezelése: %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tartalmazza(T,X) :-
    ( T == X -> true
    ; compound(T), functor(T,_,N), tart_arg_ban(N,T,X)
    ).

tart_arg_ban(N,T,X) :- 
    ( arg(N,T,A), tartalmazza(A,X) -> true
    ; N > 1, N1 is N-1, tart_arg_ban(N1,T,X)
    ).

része(T,X) :- %% Mik a lehetséges illesztések a T résztermjei és X között?
    ( T = X
    ; compound(T), functor(T,_F,N), része_arg_ban(N,T,X)
    ).

része_arg_ban(N,T,X) :- 
    ( arg(N,T,A), része(A,X)
    ; N > 1, N1 is N-1, része_arg_ban(N1,T,X)
    ).

%%% | ?- része(a(Y,nil,b(nil,2,B,nil)),X).
%%% X = a(Y,nil,b(nil,2,B,nil)) ? ;
%%% X = b(nil,2,B,nil) ? ;
%%% X = nil ? ;
%%% X = B ? ;
%%% X = 2 ? ;
%%% X = nil ? ;
%%% X = nil ? ;
%%% X = Y ? ;
%%% no

része2(T,X) :-
    ( T = X
    ; compound(T), T =.. [_|As], member_része(X,As)
    ).

member_része(X,[Y|_Xs]) :- része2(Y,X).
member_része(X,[_X|Xs]) :- member_része(X,Xs).

%% cserél(T0,X,Y,T) :-
%%     T a T0 másolata, kivéve, hogy X T0-beli
%%     előfordulásainak T-ben Y felel meg.
cserél(T0,X,Y,T) :-
    ( T0 == X -> T = Y
    ; compound(T0) ->
        functor(T0,F,N), functor(T,F,N),
        arg_csere(N,T0,X,Y,T)
    ; T = T0
    ).


arg_csere(N,T0,X,Y,T) :-
    ( N > 0 ->
        arg(N,T0,A), cserél(A,X,Y,B), arg(N,T,B),
        N1 is N-1, arg_csere(N1,T0,X,Y,T)
    ; true
    ).

arg_csere1(N,T0,X,Y,T) :- arg_csere1_(1,N,T0,X,Y,T).

arg_csere1_(I,N,T0,X,Y,T) :-
    ( I =< N ->
        arg(I,T0,A), cserél(A,X,Y,B), arg(I,T,B),
        I1 is I+1, arg_csere1_(I1,N,T0,X,Y,T)
    ; true
    ).

%% | ?- cserél(a(X,nil,b(nil,2,B,nil)),nil,[],T).
%% T = a(X,[],b([],2,B,[]))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% aA(A,B) :- B név az A név másolata, kivéve, hogy
%%            nagybetűsítettük. (á kódja 0'á, stb.)
aA(A,B) :-
    atom_codes(A,Cs), cC(Cs,Ds), atom_codes(B,Ds).

cC([],[]).
cC([C|Cs],[D|Ds]) :- bB(C,D), cC(Cs,Ds).

bB(C,D) :-
    ( 0'a=<C, C=<0'z -> D is C-0'a+0'A
    ; éÉ(C,D) -> true
    ; D = C
    ).

éÉ(0'á,0'Á). éÉ(0'é,0'É). éÉ(0'í,0'Í).
éÉ(0'ó,0'Ó). éÉ(0'ö,0'Ö). éÉ(0'ő,0'Ő).
éÉ(0'ú,0'Ú). éÉ(0'ü,0'Ü). éÉ(0'ű,0'Ű).

%% Teszt:
%% | ?- aA('ó míly gyönyörű ő',Ismeretlen).
%% Ismeretlen = 'Ó MÍLY GYÖNYÖRŰ Ő'

%%% | ?- bB(0'é,0'é).
%%% yes
%%% | ?- b_B(0'é,0'é).
%%% no

%% bB/2 helyett inkább b_B/2 -t kéne használni.

b_B(C,D) :-
    ( 0'a=<C, C=<0'z -> D is C-0'a+0'A  % is(D,+(-(C,0'a),0'A))
    ; éÉ(C,D0) -> D = D0
    ; D = C
    ).

%% number_codes/2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%% \+ Cél :- 
%%     ( Cél -> fail 
%%     ; true 
%%     ).

%% :-(\+(Cél),;(->(Cél,fail),true)).

%% :-(no(Cél),;(->(Cél,fail),true)).

%% 1+1.

%% X=X.
    
:- op( 700, xfx, =::= ).

X =::= Y :-
    ( number(X), number(Y) -> X=:=Y
    ; X==Y
    ).


% no(P) :- \+P.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getnat(N) :- skip_blanks(user), gn(0,N).

getnat_1(N) :- skip_blanks(user), gn(0,N), skip_line(user).

getnat1(N) :- skip_blanks(user), getN(N), skip1line(user).

getN(N) :-
    peek_code(user,C),
    ( 0'0 =< C, C =< 0'9 ->
      A is C-0'0, get_code(user,_), gn(A,N)
    ; skip1line(user), fail
    ).
    
gn(Acc,N) :-
    peek_code(user,C),
    ( 0'0 =< C, C =< 0'9 ->
      A is 10*Acc+C-0'0, get_code(user,_), gn(A,N)
    ; N = Acc
    ).

skip_blanks(S) :-
    peek_code(S,C),
    ( white_space(C) -> get_code(S,_), skip_blanks(S)
    ; true
    ).

white_space(0'\t).
white_space(0'\n).
white_space(0'\r).
white_space(0' ).

skip1line(S) :-
    get_code(S,C),
    ( C==0'\n -> true
    ; skip1line(S)
    ).

% get(S,C) :- get the code of the next character
%             which is greater than the blank (space) character. 
get(S,C) :-
    get_code(S,D),
    ( D > (0' ) -> C = D
    ; get(S,C)
    ).

getline(S,Cs) :-
    get_code(S,C),
    ( C == 0'\n -> Cs = []
    ; Cs = [C|Ds], getline(S,Ds)
    ).

getnum(S,N) :- getline(S,Cs), number_codes(N,Cs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% appf(F1,F2) :- F1 szövegfájl végére beszúrja F2-t.
appf(F1,F2) :-
    open(F1,append,A), open(F2,read,R),
    af(A,R),
    close(A), close(R).

af(A,R) :-
    get_code(R,C),
    ( C == -1 -> true
    ; put_code(A,C), af(A,R)
    ).


%% nl(S) :- put_code(S,0'\n).
%% XOR
%% nl(S) :- write(S,'\n').

%% flush_output(S)

%% end_of_file.

másol_pl(In,Out) :-
    open(In,read,R), open(Out,write,W),
    másol2pl(R,W),
    close(R), close(W).

másol2pl(R,W) :-
    read(R,M),
    ( M == end_of_file -> true
    ; writeq(W,M), write(W,'.\n'), másol2pl(R,W)
    ).

másol_pl_utf8(In,Out) :-
    open(In,read,R,[encoding('UTF-8')]),
    open(Out,write,W,[encoding('UTF-8')]),
    másol3pl(R,W),
    close(R), close(W).

másol3pl(R,W) :-
    repeat,
        read(R,M),
	( M \== end_of_file ->
	  numbervars(M,0,_),
	  writeq(W,M), write(W,'.\n'), fail
	; true
	)
    -> true.

% repeat.
% repeat :- repeat.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Útkeresés aciklikus gráfban: %%%

e(a,b).    e(b,c).    e(b,d). 
e(c,e).    e(d,e).    %e(d,a). 

%           a----->b----->c
%                  |      |
%                  |      |
%                  V      V
%                  d----->e

% van_út0(A,Z) : Van út A-ból Z-be az aciklikus e/2 gráfban.

van_út0(A,A).
van_út0(A,Z) :-
    e(A,B), % él egy közbenső csúcsba
    van_út0(B,Z).


% út0(A,Z,Út) : Út egy út (csúcsok listája) A-ból Z-be
%     az aciklikus e/2 gráfban.
út0(A,A,[A]).
út0(A,Z,[A|As]) :-
    e(A,B), % él egy közbenső csúcsba
    út0(B,Z,As).

%%% Útkeresés ciklikus gráfban: %%%

él(a,b).    él(b,c).    él(b,d).
él(c,a).    él(c,e).    él(d,e).    él(e,e).

%             __________
%            /          \
%           V            \
%           a----->b----->c
%                  |      |
%                  |      |
%                  V      V
%                  d----->e

% van_út(A,Z) :-
%     Az él/2 (irányított kört tartalmazó) gráfban van út A-ból Z-be.
van_út(A,Z) :- van_útja(A,[],Z).

% van_útja(A,Voltak,Z) :-
%     A-ból el lehet jutni a Voltak csúcslista érintése nélkül Z-be.
van_útja(A,_Voltak,A).
van_útja(A,Voltak,Z) :-
    él(A,B), % él egy közbenső csúcsba
    B\=A,             % nem hurokél
    \+member_(B,Voltak), % nem ciklus
    van_útja(B,[A|Voltak],Z).


% út(A,Z,Út) :-
%     Út egy körmentes út (csúcsok listája) A-ból Z-be.
út(A,Z,Út) :- útja(A,[],Z,Út).

% útja(A,Voltak,Z,Út) :-
%     A Voltak csúcslista fordítottjának és egy A-ból Z-be
%     vezető útvonalnak a konkatenáltja az Út körmentes út.
útja(A,Voltak,A,Út) :- reverse([A|Voltak],Út).
útja(A,Voltak,Z,Út) :-
    él(A,B), % él egy közbenső csúcsba
    B\=A,             % nem hurokél
    \+member_(B,Voltak), % nem ciklus
    útja(B,[A|Voltak],Z,Út).


%%% Önmódosító programok: %%%

% út1(@A,@Z,-Út) : Út egy út (csúcsok listája) A-ból Z-be
%     a ciklikus él/2 gráfban.
% Előf: A alapterm, nonvar(Z).

:- dynamic volt/1.

%% :- op(1150,fx,dynamic).

%% :- dynamic((volt/1,x/2,y/3,z/2)).
%% :- dynamic volt/1, x/2, y/3, z/2.
%% :- dynamic (volt/1, x/2, y/3, z/2).

út1(A,Z,Út) :-
    ground(A), nonvar(Z), %% Kivételesen ellenőrizzük az előfeltételt.
    retractall(volt(_)),
    asserta(volt(A)),
    út1_(A,Z,Út), !.      %% piros vágó
    
út1_(A,A,[A]).
út1_(A,Z,[A|As]) :-
    él(A,B),    % él egy közbenső csúcsba
    \+ volt(B),    % Itt még nem jártunk.
    asserta(volt(B)),    % Most már igen.
    út1_(B,Z,As).

% asserta(Klóz), assertz(Klóz),

% retract(Klóz)
%
% retractall(Klózfej) :- retract((Klózfej:-_)), fail ; true.

retractall_(Klózfej) :- retract((Klózfej:-_)), fail ; true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Előf: F egy szintaktikusan helyes, utf-8 kódolású Prolog forrásfájl.
%%
%% betölt_utf8(F) :-
%%     Beolvassuk F klózait, és
%%     dinamikus predátumokat hozunk létre belőlük.
%%     A deklarációkat, a direktívákat és a DCG szabályokat
%%     figyelmen kívül hagyjuk.

betölt_utf8(F) :-
    open(F,read,Cs,[encoding('UTF-8')]),
    töltő_ciklus_utf8(Cs),
    close(Cs).

töltő_ciklus_utf8(Cs) :-
    read(Cs,Klóz),
    ( Klóz == end_of_file -> true
    ; Klóz = :-(_) -> töltő_ciklus_utf8(Cs)
    ; Klóz = (_-->_) -> töltő_ciklus_utf8(Cs)
    ; assertz(Klóz), töltő_ciklus_utf8(Cs)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retract_all(P) :- retract((P:-_)), fail.
retract_all(_P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(megoldás/1).

find_all(X,Cél,_Xs) :-
    retractall(megoldás(_)), % tisztázás
    Cél, assertz(megoldás(X)), fail.
find_all(_X,_Cél,Xs) :- begyűjt(Xs).

begyűjt(Ys) :-
    ( retract(megoldás(X)) -> Ys = [X|Xs], begyűjt(Xs)
    ; Ys = []
    ).

%%% | ?- findall(Xs+Ys,append(Xs,Ys,[1,2,3]),Zs).
%%% Zs = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]] ? ;
%%% no
%%% | ?- find_all(Xs+Ys,append(Xs,Ys,[1,2,3]),Zs).
%%% Zs = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]] ? ;
%%% no
%%% | ?- find_all(Zs+Ys,(append(Xs,Ys,[1,2,3]),findall(Z,member(Z,Xs),Zs)),Us).
%%% Us = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]] ? ;
%%% no
%%% % source_info
%%% | ?- find_all(Zs+Ys,(append(Xs,Ys,[1,2,3]),find_all(Z,member(Z,Xs),Zs)),Us).
%%% Us = [[1,2,3]+[]] ? ;
%%% no

%% HF: find_all/3 hibájának a kijavítása, a blackboard használata nélkül.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

párosak1([],[]).
párosak1([X|Xs],Ps) :-
    ( X mod 2 =:= 0 -> Ps = [X|Ys], párosak1(Xs,Ys)
    ; párosak1(Xs,Ps)
    ).

párosak2(Xs,Ps) :- find_all( X, (member(X,Xs), X mod 2 =:=0), Ps ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

szülei(X,Ys) :- findall(Y,szülője(Y,X),Ys).
gyermekei(X,Ys) :- findall(Y,szülője(X,Y),Ys).

testvér(X,Y) :- szülője(Z,X), szülője(Z,Y), X\==Y.

%% testvérei_(X,Ys) :- X testvérei Ys elemei.
testvérei_(X,Ys) :- findall(Y,testvér(X,Y),Ys).

%% gyűjt(Mit,Honnan,Mibe) :- 
%%     Honnan megoldásainak halmaza Mit szerint Mibe.
gyűjt(Mit,Honnan,Mibe) :-
    findall(Mit,Honnan,Hová), sort(Hová,Mibe).

%% testvérei(X,Ys) :- X testvéreinek halmaza Ys.
testvérei(X,Ys) :- gyűjt(Y,testvér(X,Y),Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% :- module( mastermind, [ master/0, mastermind/0 ] ).

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
    digit(A,0), digit(B,0), B\==A,
    digit(C,0), C\==A, C\==B,
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
    type_seq([Tip,'      ',Score,'\n']), 
    fail.
type_history.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Kivételkezelés: %%%

empty_stack([]).

push(Xs,X,[X|Xs]).

pop([X|Xs],X,Xs).
pop([],_,_) :- throw(empty_stack_error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% olvas(Term) :- A szabványos bemenetről olvas, mígnem szintaktikusan helyes
%%     Prolog termet kap. A Term termet ponttal és fehér szóközzel kell lezárni.
%%     Az aktuális bemenő sor esetleges maradékát figyelmen kívül hagyja.
olvas(Term) :-
    catch( ( read(user,Term), skip_line(user) ) ,
	   error(syntax_error(IsoHiba),_SICStusHiba),
	   ( ír(syntax_error(IsoHiba)), %ír(_SICStusHiba),
	     olvas(Term)
	   )
	 ).

ír(Term) :- numbervars(Term,0,_), writeq(user,Term), nl(user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Saját, egyszerűsített Prolog shell:

pl :-
    prolog_célt_olvas(Cél),
    ( Cél == halt -> true
    ; ground(Cél) -> alapcél_megoldó(Cél), pl
    ; cél_megoldó(Cél), pl
    ).

alapcél_megoldó(Cél) :-
    catch( (Cél -> ír(igen) ; ír(nem) ),
	   Error,
	   ( Error = error(Hiba,_) -> ír(Hiba)
	   ; ír(Error)
	   )
	 ).

cél_megoldó(Cél) :-
    catch( ( Cél, ír(Cél), fail ; ír('Nincs (több) megoldás') ),
	   Error,
	   ( Error = error(Hiba,_) -> ír(Hiba)
	   ; ír(Error)
	   )
	 ).

prolog_célt_olvas(Cél) :-
    prompt(P,'?- '), olvas(Cél), prompt(_,P).

%% exit :- halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Naiv sor típus megvalósítás:

empty0([]).

add0([],X,[X]).
add0([Y|Ys],X,[Y|Zs]) :- add0(Ys,X,Zs).

rem0([X|Xs],X,Xs).

%% Sorok dupla veremmel:

empty(d([],[])).

add(d(Xs,Ys),E,d(Xs,[E|Ys])).

rem(d(Xs,Ys),E,ResultQueue) :-
    ( Xs = [Z|Zs] -> E = Z, ResultQueue = d(Zs,Ys)
    ; % Xs == [], Ys \== [],
      reverse(Ys,[E|Us]), ResultQueue = d(Us,[])
    ).

%%%%%%%%%%%%%% D-LISTÁK %%%%%%%%%%%%%

%% [1,2,3|Z]    % parciális (részleges) lista, ahol var(Z)
%% [1,2,3|[]] == [1,2,3] % valódi lista
%% [1,2,3|[4,5]] == [1,2,3,4,5] % valódi lista
%% [1,2,3|[4,5|W]] == [1,2,3,4,5|W] % parciális (részleges) lista (var(W))
%% [1,2,3|4] == .(1,.(2,.(3,4))) % nem lista
%% Z    % parciális (részleges) lista, ahol var(Z)

%% Az [1,2,3] valódi lista néhány d-lista reprezentációja
%% (A D-LISTA NEM NYELVI ELEM!):
%%
%% [1,2,3|Z]-Z
%% [1,2,3]-[]
%% [1,2,3,4]-[4]
%% [1,2,3,4|Z]-[4|Z] == [1,2,3|[4|Z]]-[4|Z]
%% Nem D-lista: [1,2,3|4]-4
%%
%% Üres d-lista: Z-Z alakú, ahol Z parciális (részleges) vagy valódi lista.

%% append_dl(As,Bs,Cs): The difference-list Cs is the
%%     result of appending Bs to As, where As and Bs are
%%     compatible difference-lists.

append_dl(Xs-Ys,Ys-Zs,Xs-Zs).

% Működés: Tegyük fel az alábbi kérdést:
%% ?- append_dl( [X1,X2,...,Xn|Us]-Us, [Y1,Y2,...,Ym|Zs]-Zs, Vs ).
%%
%% Us=Ys  hatása:
%% Xs = [X1,X2,...,Xn|[Y1,Y2,...,Ym|Zs]]
%% azaz:
%% Xs = [X1,X2,...,Xn,Y1,Y2,...,Ym|Zs]
%% Ezután  Vs=Xs-Zs  hatása:
%% Vs = [X1,X2,...,Xn,Y1,Y2,...,Ym|Zs] - Zs

%% member_dl(X,XS) :- X az Xs d-lista eleme.
member_dl(X,[X|Ys]-Zs) :- [X|Ys] \== Zs.
member_dl(X,[Y|Ys]-Zs) :- [Y|Ys] \== Zs, member_dl(X,Ys-Zs).

%% reverse_dl(As,Bs) :- Az As d-lista fordítottja a Bs d-lista.
reverse_dl(As-Xs,Bs-Ys) :-
    ( As==Xs -> Bs=Ys
    ; As=[C|Cs], reverse_dl(Cs-Xs,Bs-[C|Ys])
    ).

%% reverse1(As,Bs) :- Az As valódi lista fordítottja a Bs valódi lista.
reverse1(Xs,Ys) :- reverse_dl(Xs-[],Ys-[]).

%% flatten(@Xs,?Ys): Sorban az Xs összetett lista 
%%     felső szintű elemeit és rekurzívan az
%%     allistáinak elemeit tartalmazza Ys.
%% Előf: Xs valódi lista. Ys lista (esetleg var).

flatten(Xs,Ys) :- flatten_dl(Xs,Ys-[]).

flatten_dl(X,[X|Xs]-Xs) :-
    \+ lst(X), !.
flatten_dl([],Xs-Xs).
flatten_dl([X|Xs],Ys-Zs) :-
    flatten_dl(X,Ys-Ys1), lst(Xs), flatten_dl(Xs,Ys1-Zs).

lst(X) :- var(X), !, fail.
lst([]).
lst([_|_]).

%% reverse2(Xs,Ys): Ys is the reversal of the list Xs.

reverse2(Xs,Ys) :- reverse2_dl(Xs,Ys-[]).
	
reverse2_dl([X|Xs],Ys-Zs) :-
    reverse2_dl(Xs,Ys-[X|Zs]).
reverse2_dl([],Xs-Xs).

%% Queue-handling with difference-pair lists .

%%emptyq(Queue): Queue egy üres sor (Var-Var alakú d-lista)
emptyq(Q-Q) :- var(Q).

%% addq(+InQ,?ITEM,-ResQ): Inq d-lista (sor) végére téve ITEM-et
%%    adódik a Resq d-lista (sor), var kivonandóval.
%% Elöf: InQ egy d-lista var kivonandóval.
addq(Q1-[ITEM|Y],ITEM,Q1-Y).

%% ?- addq([P1,P2,...,PN|Z]-Z,ITEM,R).
%% Q1 = [P1,P2,...,PN|Z], Z = [ITEM|Y], R = Q1-Y
%% => R = [P1,P2,...,PN|[ITEM|Y]]-Y = [P1,P2,...,PN,ITEM|Y]-Y

%% addq(Q1-X,ITEM,Q2) :- X = [ITEM|Y], Q2 = Q1-Y.

remq([H|T]-Z,H,T-Z) :- 
    [H|T]\==Z.

%%% remq([H|T]-Z,H,T-Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% hanoi(@N,@A,@B,@C,-Ms): Ms a mozgatások sorozata, ami ahhoz 
%%     szükséges, hogy a "Hanoi tornyai" puzzle szabályai szerint
%%     N db korongot áttegyünk az A tüskéről a B-re
%%     a C segítségével.
%% Előf: N pozitív egész szám.

%%%%%%%%%%%%%%%%% Naive hanoi: %%%%%%%%%%%%%%%%%

%% Just hanoi0/2 is to be called from prompt.
hanoi0(N,Ms) :-
    integer(N), N>0,
    hanoi0(N,1,2,3,Ms).

hanoi0(N,A,B,C,Ms) :-
    N > 1,
    N1 is N - 1,
    hanoi0(N1,A,C,B,Ms1),
    append(Ms1,[(N:A->B)],Ms2),
    hanoi0(N1,C,B,A,Ms3),
    append(Ms2,Ms3,Ms).
hanoi0(1,A,B,_,[(1:A->B)]).

%%%%%%%%%%%%%%%%% better hanoi: %%%%%%%%%%%%%%%%%

%% Just hanoi1/2 is to be called from prompt.
hanoi1(N,Ms) :-
    integer(N), N>0,
    hanoi1(N,1,2,3,Ms).

hanoi1(N,A,B,C,Ms) :-
    N > 1,
    N1 is N - 1,
    hanoi1(N1,A,C,B,Ms1),
    append(Ms1,[(N:A->B)|Ms2],Ms),
    hanoi1(N1,C,B,A,Ms2).  
hanoi1(1,A,B,_,[(1:A->B)]).

%%%%%%%%%%%%%%%%% hanoi with d-lists: %%%%%%%%%%%%%%%%%

%% Just hanoiD/2 is to be called from prompt.
hanoiD(N,Ms) :-
    integer(N), N>0,
    hanoiD(N,1,2,3,Ms,[]).

hanoiD(N,A,B,C,Ms,V) :-
    N > 1,
    N1 is N - 1,
    hanoiD(N1,A,C,B,Ms,V1),
    V1=[(N:A->B)|Ms2],
    hanoiD(N1,C,B,A,Ms2,V).  
hanoiD(1,A,B,_,[(1:A->B)|V],V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Compiled code:

%%% | ?- ms(hanoiD(23,_),M).
%%% M = 719
%%% | ?- ms(hanoi1(23,_),M).
%%% M = 1109
%%% | ?- ms(hanoi0(23,_),M).
%%% M = 1624
%%% | ?- ms(hanoiD(24,_),M).
%%% ! Resource error: insufficient memory

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in(I,_J,I).
in(I,J,K) :- I < J, I1 is I+1, in(I1,J,K).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qs(Xs,Ys) :- quicksort(Xs,Ys,[]).

quicksort([X|Xs]) -->
    {divide(Xs,X,Ls,Gs)},
    quicksort(Ls), [X], quicksort(Gs).
quicksort([]) --> [].

divide([X|Xs],Y,Ls,Gs) :-
    ( X =< Y -> Ls = [X|Ks], divide(Xs,Y,Ks,Gs)
    ; Gs = [X|Ns], divide(Xs,Y,Ls,Ns)
    ).
divide([],_Y,[],[]).
