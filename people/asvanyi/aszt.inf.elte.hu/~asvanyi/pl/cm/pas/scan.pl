% Program to scan a file and assemble identifiers,
% sequences of special characters, numbers and delimiters.

:- module( scan, [scan/2] ).

scan(File,List) :-
    open(File,read,Stream), set_input(Stream),
    get_code(C1), reap_tokens(C1,List),
    close(Stream).

reap_tokens(C1,List) :-
    read_token(C1,Token,C2),
    reap_tokens_2(Token,C2,List).

reap_tokens_2(X,C,[X|Xs]) :- X \= end_of_file, !, reap_tokens(C,Xs).
reap_tokens_2(end_of_file,_C,[]).

read_token(C1,Token,C2) :-
    char_type(C1,Type), read_symbol(Type,C1,Token,C2).

char_type(C,Type) :- char_type0(C,Type0) -> Type=Type0 ; Type = special.

% char_type0(X,special) :- member(X,"$^&*-+=~#@:/?><\\"). % etc...

char_type0(X,letter) :- X>=0'A, X=<0'Z ; X>=0'a, X=<0'z ; X=:=0'_ .
char_type0(X,digit) :- X>=0'0, X=<0'9.
char_type0(X,separator) :- member(X,",;(){}[]").
char_type0(X,layout) :- 0=<X, X=<32.
char_type0(-1,eof).		% code of end_of_file character

read_symbol(letter,C1,T,C2) :-
    read_symbol_tail(identifier,Cs,C2),
    atom_codes(T,[C1|Cs]).
read_symbol(digit,C1,T,C2) :-
    read_symbol_tail(integer,Cs,C2),
    number_codes(T,[C1|Cs]).
read_symbol(separator,C1,T,C2) :-
    char_code(T,C1),
    get_code(C2).
read_symbol(special,C1,T,C2) :-
    read_symbol_tail(operator,Cs,C2),
    atom_codes(T,[C1|Cs]).
read_symbol(layout,_,T,C2) :-
    get_code(C1),
    read_token(C1,T,C2).
read_symbol(eof,_,end_of_file,_).

read_symbol_tail(SymbolType,Codes,NextCode) :-
    get_code(C), char_type(C,CharType),
    ( allowed_in(SymbolType,CharType) ->
      Codes = [C|Cs], read_symbol_tail(SymbolType,Cs,NextCode)
    ; Codes = [], NextCode = C
    ).

allowed_in(identifier,letter).
allowed_in(identifier,digit).
allowed_in(integer,digit).
allowed_in(operator,special).
