%%% a   = 1+1. 
%%% a_1 = a-1.   
%%% b   = 3*a.    
%%% c3  = b//a.       %% integer division 
%%% d   = b mod 4.

%%% u = u*4.    %% u depends on undefined constant (not allowed)
%%% d = a+1.    %% d is redefined constant (not allowed)

:- use_module( scan, [scan/2] ).

constants --> constdef, !, constants.
constants --> [].

constdef --> const, [=], expr, ['.'].

expr --> operand, operator, operand.

operator --> [Op], { op(Op) }.
op(+).    op(-).    op(*).    op(//).    op(mod).

operand --> [Y], { integer(Y), ! }.
operand -->  id.

const --> id.
id --> [Id], {is_id(Id)}.

is_id(Id) :- 
    atom(Id), atom_codes(Id,[C1|Cs]), lower_case_letter_code(C1),
    \+ ( member(K,Cs), \+id_code(K) ).

id_code(K) :-
    ( lower_case_letter_code(K) -> true
    ; upper_case_letter_code(K) -> true
    ; digit_code(K) -> true
    ; K == 0'_
    ).

lower_case_letter_code(K) :- 0'a =< K, K =< 0'z.
upper_case_letter_code(K) :- 0'A =< K, K =< 0'Z.
digit_code(K) :- 0'0 =< K, K =< 0'9.

/*
| ?- _Tokens = [a,=,1,+,1,., a_1,=,a,-,1,., b,=,3,*,a,.,
                 c3,=,b,//,a,., d,=,b,mod,4,.],
     constants(_Tokens,[]).
yes
*/