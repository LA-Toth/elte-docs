:- module( d3, [ semantics/3, scan/2, constants/3 ] ).

semantics(File,Tokens,Semantics) :-
    scan(File,Tokens), %% tokenizing
    constants(Semantics,Tokens,[]).
%% Semantics is the list of the
%%  - constants with their values and
%%  - error terms for erronous constant defs.

constants(Constants) --> constants([],Cs), 
    {reverse(Cs,Constants)}.

:- use_module( scan, [scan/2] ).
:- use_module( library(lists), [reverse/2] ).

constants(Bs,Cs) --> constdef(Bs,C), !, constants([C|Bs],Cs).
constants(Bs,Cs) --> nonempty(Tokens), !,
                     {Cs=[error(constants,Tokens)|Bs]}.
constants(Bs,Bs) --> [].

nonempty([T|Ts]) --> [T], anything(Ts).
anything([T|Ts]) --> [T], !, anything(Ts).
anything([]) --> [].

constdef(Bs,C) -->
    const(Bs,Id), [=], expr(Bs,Expr), ['.'], !,
    { atom(Id), catch( Val is Expr, _, fail ) -> C = (Id=Val)
    ; atom(Id) -> C = error(expression,Id=Expr)
    ; catch( Val is Expr, _, fail ) -> C = error(constant,Id=Val)
    ; C = error(constant_and_expression,Id=Expr)
    }.
constdef(_Bs,error(constdef_syntax,Ts)) -->  % Basic problem with the
    constdef_error(Ts).                      % constant definition.

constdef_error(['.']) --> ['.'], !.
constdef_error([X|Xs]) --> [X], constdef_error(Xs).

expr(Bs,Expr), ['.'] -->
    operand(Bs,X), operator(Op), operand(Bs,Y), ['.'], !,
    { atom(Op) -> Expr =.. [Op,X,Y]      % Op is a legal operator
    ; Expr = [X,Op,Y]                    % Op is not a legal operator
    }.
expr(_Bs,error(expression_syntax,Ts)) -->  % Basic problem with the
    expression_error(Ts).                  % struct of the expression

expression_error([]), ['.']   --> ['.'], !.
expression_error([X|Xs]) --> [X], !, expression_error(Xs).
%% expression_error([]) --> [].

operator(Operator) --> [Op],
    { op(Op) -> Operator = Op
    ; Operator = error(operator,Op)
    }.

op(+).    op(-).    op(*).      op(//).    op(mod).

operand(_Bs,X) --> [Y], { integer(Y), !, X = Y }.
operand(Bs,X) -->  id(Y), !,
    { member(Y=Z,Bs) -> X = Z
    ; X = error(undefined,Y)
    }.
operand(_Bs,X) --> [Y], { X = error(operand_syntax,Y) }.

const(Bs,Id) --> id(Y), !,
    { member(Y=_Z,Bs) -> Id = error(redefined,Y)
    ; Id = Y
    }.
const(_Bs,error(non_id_constant_symbol,X)) --> [X].

id(Id) --> [Id], {is_id(Id)}.    % Predicate  is_id/1  is unchanged.


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
 | ?- _Tokens = [a,=,1,+,1,., 'A1',=,a,-,1,., b,=,3,*,'A',., 
                 c3,=,b,//,a,., a,=,b,rem,4,., 6,=,b,.],
      constants(Defs,_Tokens,[]).
*/
%%% Defs = [a=2,
%%%         error(constant,error(non_id_constant_symbol,'A1')=1),
%%%         error(expression,b=3*error(operand_syntax,'A')),
%%%         error(expression,c3=error(undefined,b)//2),
%%%         error(constant_and_expression,
%%%               error(redefined,a)=
%%%               [error(undefined,b),error(operator,rem),4]),
%%%         error(constant_and_expression,
%%%               error(non_id_constant_symbol,6)=
%%%               error(expression_syntax,[b]))]
