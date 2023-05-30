% Parsing a tokenlist of a Simple Pascal program.
% Discovering a syntax error using default rules.

:- module( parse, [parse/2] ).

parse(Tokens,Structure) :- program(Structure,Tokens,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program(Struct) --> preface, block(Struct,[]), epilogue.

preface --> [begin], !.
preface --> [_], preface.

epilogue --> [.], !, anything.
epilogue --> [].

anything --> [_], !, anything.
anything --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block(Struct,Vs) -->
    declaration_part(Vs,Ws), statement_list(Struct,Ws).

declaration_part(Vs,Ws) --> [int], !, declarations(Vs,Ws).
declaration_part(Vs,Vs) --> [].

declarations(Vs,Ws) -->
    variable(V), {\+member(V,Vs)},
    rest_variables([V|Vs],Ws).

rest_variables(Vs,Ws) --> [','], !, declarations(Vs,Ws).
rest_variables(Vs,Vs) --> [;].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ss is the list representing the syntax tree of the statement part.

statement_list([],_Vs) --> [end], !.
statement_list([S|Ss],Vs) -->
    statement(S,Vs), !,
    rest_statements(Ss,Vs).

rest_statements(Ss,Vs) --> [;], !, statement_list(Ss,Vs).
rest_statements([],_) --> [end].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statement(Ss,Vs) --> 
    [begin], !, block(Ss,Vs).
statement(assign(X,E),Vs) --> 
    variable(X), [:=], !,
    { declared(X,Vs) },
    expression(E,Vs).
statement(if(T,S1,S2),Vs) -->
    [if], test(T,Vs), [then], statement(S1,Vs), [else], !, statement(S2,Vs).
statement(while(T,Ss),Vs)  --> 
    [while], test(T,Vs), [do], !, statement(Ss,Vs).
statement(read(X),Vs)     -->
    [read], !, variable(X), { declared(X,Vs) }.
statement(write(X),Vs)    -->
    [write], !, expression(X,Vs). 
statement([],_), [X] -->         % empty statetement
    [X], {end_of_statement(X)}, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(compare(Op,X,Y),Vs) -->
    expression(X,Vs), comparison_op(Op), expression(Y,Vs), test_end.

expression(Expr,Vs) -->
    sub_expression(2,Expr,Vs), expression_end.

sub_expression(N,E,Vs) -->
    { N>0, N1 is N-1 },
    sub_expression(N1,E1,Vs), rest_expression(N,E1,E,Vs).
sub_expression(0,E,Vs) -->
    ['('], sub_expression(2,E,Vs), [')'] ; atomic_expression(E,Vs).

atomic_expression(number(X),_) -->
    constant(X).
atomic_expression(name(X),Vs) -->
    variable(X),
    { declared(X,Vs) }.

rest_expression(N,E1,E,Vs) -->
    arith_operator(N,Op),
    { N1 is N-1 },
    sub_expression(N1,E2,Vs),
    rest_expression(N,expr(Op,E1,E2),E,Vs).
rest_expression(_,E,E,_) --> [].

expression_end, [X] --> [X], {end_of_expression(X)}.

test_end, [X] --> [X], {end_of_test(X)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arith_operator(2,+) --> [+].
arith_operator(2,-) --> [-].
arith_operator(1,*) --> [*].
arith_operator(1,/) --> [/].

comparison_op(=) --> [=].
comparison_op(\=) --> [\=].
comparison_op(>) --> [>].
comparison_op(<) --> [<].
comparison_op(>=) --> [>=].
comparison_op(=<) --> [=<].

constant(X) --> [X], { integer(X), X>=0 }.

variable(V) --> identifier(V).

identifier(X) -->
    [X],
    { atom(X), atom_codes(X,[X1|Xs]),
      letter_type(X1), \+ ( member(Y,Xs), \+alphanum_type(Y) )
    }. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declared(X,Vs) :- member(X,Vs).

letter_type(X) :-
    ( X>=0'A, X=<0'Z -> true ; X>=0'a, X=<0'z -> true ; X=:=0'_ ).

digit_type(X) :- X>=0'0, X=<0'9.

alphanum_type(X) :- ( letter_type(X) -> true ; digit_type(X) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end_of_statement(;).
end_of_statement(end).
end_of_statement(else).

end_of_test(do).
end_of_test(then).

end_of_expression(X) :-
    ( end_of_statement(X) -> true
    ; end_of_test(X) -> true
    ; comparison_op(X,_,_)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
