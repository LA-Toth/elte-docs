%% -*- Mode: Prolog; coding: utf-8 -*- 

:- module(meta,[for_all/2,find_all/3]).

:- meta_predicate for_all(:,:), find_all(?,:,?).

for_all(X,Y) :- \+ (X,\+Y).

%% http://aszt.inf.elte.hu/~asvanyi/pl/art/s4.pl

%% http://www.sics.se/isl/sicstus/license4-academic.html
%%                                             (EXHIBIT D)

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(100)]).

:- set_prolog_flag(legacy_char_classification,on).


% Írjuk meg findall/3-mal ekvivalens a find_all/3 predikátumot!
%
% Előf: Cél egy Prolog cél(sorozat), véges keresési fával.
%
% Mj.: Rendesen X a Cél egyik változója, vagy egy olyan összetett term,
%      aminek ismeretlenjei a Cél változói.
%
% find_all(X,Cél,Xs) :- X behelyettesítései a Cél megoldásai szerint
%     adják az Xs valódi listát, a megoldások sorrendjének megfelelően.
%
% Részleges megoldás:

%%% :- dynamic(megoldás/1).

%%% find_all_0(X,Cél,_Xs) :-
%%%     retractall(megoldás(_)), % tisztázás, ha egy régi Cél elszállt volna...
%%%     Cél, assertz(megoldás(X)), fail.
%%% find_all_0(_X,_Cél,Xs) :- 'begyűjt_0'(Xs).

%%% 'begyűjt_0'(Ys) :-
%%%     ( retract(megoldás(X)) -> Ys = [X|Xs], 'begyűjt_0'(Xs)
%%%     ; Ys = []
%%%     ).

% A find_all_0/3 a beépített findall/3-hoz hasonló:
%
% Egy lista szétvágásai 2 részre:
% | ?- find_all_0(Xs+Ys,append(Xs,Ys,[1,2,3]),As). 
% As = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]]
%
% A fenti összeget az alábbi csavarral is megkaphatjuk:
% | ?- find_all_0(Zs+Ys,(append(Xs,Ys,[1,2,3]),findall(Z,member(Z,Xs),Zs)),As).
% As = [[]+[1,2,3],[1]+[2,3],[1,2]+[3],[1,2,3]+[]]
%
% A beágyazott find_all_0 hívás most nem várt módon működik:
% | ?- find_all_0(Zs+Ys,(append(Xs,Ys,[1,2,3]),find_all_0(Z,member(Z,Xs),Zs)),
%                                                                         As).
% As = [[1,2,3]+[]]

% Feladat: Definiáljuk a find_all/3-at a beépített findall/3-nak megfelelően!
% A megoldásban ne használjuk az előre definiált vagy könyvtári megoldás
% összegyűjtő predikátumok egyikét sem!

% Megoldás:
% (Feltesszük, hogy a Cél nem használja sem a meta:megoldás/2 predikátumot,
%  sem a meta:find_all_sorszám feketetábla kulcsot,
%  amiket a findall modul normál használat esetén eltakar.)

:- dynamic(megoldás/2).

:- bb_put(find_all_sorszám,1).

find_all(X,Cél,Xs) :-
    bb_get(find_all_sorszám,I),
    I1 is I+1, bb_put(find_all_sorszám,I1),    
    (
      retractall(megoldás(I,_)), % tisztázás, ha egy régi Cél elszállt volna...
      catch(Cél,Exception,(bb_put(find_all_sorszám,I),throw(Exception))),
      assertz(megoldás(I,X)), fail
    ;
      'begyűjt'(I,Xs), bb_put(find_all_sorszám,I)
    ).

'begyűjt'(I,Ys) :-
    ( retract(megoldás(I,X)) -> Ys = [X|Xs], 'begyűjt'(I,Xs)
    ; Ys = []
    ).
