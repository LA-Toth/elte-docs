% Constants is the list representing the semantics.
constants(Constants) --> constants([],Cs), {reverse(Cs,Constants)}.

:- use_module( scan, [scan/2] ).
:- use_module( library(lists), [reverse/2] ).


% List Bs represents the semantics of the constants defined up this point --
% in reversed order, and C is the semantics of the actual constant.
constants(Bs,Cs) --> constdef(Bs,C), !, constants([C|Bs],Cs).
constants(Bs,Bs) --> [].

constdef(Bs,Id=Val) -->
    const(Bs,Id), [=], expr(Bs,Expr), ['.'], 
    { catch( Val is Expr, _, fail ) }.
    % In case of zero divisor or arithmetic overflow, this rule must fail.


expr(Bs,Expr) -->
    operand(Bs,X), operator(Op), operand(Bs,Y), !,
    { Expr =.. [Op,X,Y] }.
% X and Y are the values of the operands.


operator(Op) --> [Op], { op(Op) }.
op(+).    op(-).    op(*).    op(//).    op(mod).

operand(_Bs,X) --> [Y], { integer(Y), !, X = Y }.
operand(Bs,X) -->  id(Y), { member(Y=Z,Bs) -> X = Z }.


const(Bs,Id) --> id(Id), { \+member(Id=_Z,Bs) }.

id(Id) --> [Id], {is_id(Id)}.    % Predicate  is_id/1  is unchanged.

/*
 | ?- _Tokens = [a,=,1,+,1,., a_1,=,a,-,1,., b,=,3,*,a,.,
                 c3,=,b,//,a,., d,=,b,mod,4,.],
      constants(Defs,_Tokens,[]).
 Defs = [ a=2, a_1=1, b=6, c3=3, d=2 ]
*/

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
