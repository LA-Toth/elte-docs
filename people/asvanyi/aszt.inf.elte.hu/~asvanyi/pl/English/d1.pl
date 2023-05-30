constants --> constants([]).

:- use_module( scan, [scan/2] ).

% Bs is the list of constant identifiers defined up till now.
% C is the identifier of the first constant described by this very rule:
constants(Bs) --> constdef(Bs,C), !, constants([C|Bs]).
constants(_Bs) --> [].

constdef(Bs,Id) --> const(Bs,Id), [=], expr(Bs), ['.'].

expr(Bs) --> operand(Bs), operator, operand(Bs).

operator --> [Op], { op(Op) }.
op(+).    op(-).    op(*).    op(//).    op(mod).

operand(_Bs) --> [Y], { integer(Y), ! }.
% If an operand of an expression is an id, it must have been defined before:
operand(Bs) -->  id(Y), { member(Y,Bs) -> true }.

% The constant should not have been defined before:
const(Bs,Id) --> id(Id), { \+member(Id,Bs) }.

id(Id) --> [Id], {is_id(Id)}.    % Predicate  is_id/1  is unchanged.

is_id(Id) :- 
    atom(Id), atom_codes(Id,[C1|Cs]), lower_case_letter_code(C1),
    \+ ( member(K,Cs), \+id_code(K) ).
%%  (\+ member(K,Cs) ; id_code(K)) %! does not work!

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