% Parsing a tokenlist of a Simple Pascal program.
% Discovering a syntax error using default rules.

:- module( parse, [parse/1] ).

parse(Tokens) :- program(Structure,Tokens,[]).

program --> preface, block, epilogue.

preface --> [begin], !.
preface --> [_], preface.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block --> declaration_part, statement_list.

declaration_part --> [int], !, declarations.
declaration_part --> [].

declarations(Vs,Ws,Es) -->
    variable(V), !,
    ( {member(V,Vs)} ->
      {Es=[double_declaration(V)|Fs]},
      rest_variables(Vs,Ws,Fs)
    ; rest_variables([V|Vs],Ws,Es)
    ),
    !.
declarations(Vs,Vs,Es) -->
    declaration_error(L),
    { L = [X|_] -> Es = [non_identifier_declared_as_variable(X)]
    ; Es = [program_is_not_finished]
    }.

rest_variables(Vs,Ws,Es) --> [','], !, declarations(Vs,Ws,Es).
rest_variables(Vs,Ws,Es) --> [;], !, { Ws=Vs, Es=[] }.
rest_variables(Vs,Vs,Es) -->
    declaration_error(L),
    { L = [X|_] -> Es = [missing_colon_or_semicolon(X)]
    ; Es = [declaration_is_not_finished]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ss is the list representing the syntax tree of the statement part.

statement_list([],_V) --> [end], !.
statement_list([S|Ss],V) -->
    statement(S,V), !,
    rest_statements(Ss,V).

rest_statements(Ss,V) --> [;], !, statement_list(Ss,V).
rest_statements([],_) --> [end], !.
rest_statements([syntax_error(statement_separator(X))|Ss],V) -->
    statement_error(X), statement_list(Ss,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statement(Ss,V) --> 
    [begin], !, block(Ss,V).
statement(assign(Y,E),V) --> 
    variable(X), [:=], !,
    { declared(X,V) -> Y=X ; Y=syntax_error(undeclared(X)) },
    expression(E,V).
statement(if(T,S1,S2),V) -->
    [if], test(T,V), [then], statement(S1,V), [else], !, statement(S2,V).
statement(while(T,Ss),V)  --> 
    [while], test(T,V), [do], !, statement(Ss,V).
statement(read(Y),V)     -->
    [read], !,
    ( variable(X) -> { declared(X,V) ->  Y=X ; Y=syntax_error(undeclared(X)) }
    ; statement_error(X), {Y=syntax_error(variable(X))}
    ).
statement(write(X),V)    -->
    [write], !, expression(X,V). 
statement([],_), [X] -->         % empty statetement
    [X], {end_of_statement(X)}, !.
statement(syntax_error(statement(L)),_) --> 
    statement_error(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(compare(Op,X,Y),V) -->
    expression(X,V), comparison_op(Op), expression(Y,V), test_end, !.
test(syntax_error(test_error(L)),_V) -->
    test_error(L).

expression(Expr,V) -->
    sub_expression(2,Expr,V), expression_end, !.
expression(syntax_error(expression(L)),_V) -->
    expression_error(L).

sub_expression(N,E,V) -->
    { N>0, N1 is N-1 },
    sub_expression(N1,E1,V), rest_expression(N,E1,E,V).
sub_expression(0,E,V) -->
    ['('], sub_expression(2,E,V), [')'] ; atomic_expression(E,V).

atomic_expression(number(X),_) -->
    constant(X).
atomic_expression(name(Y),V) -->
    variable(X),
    { declared(X,V) ->  Y=X ; Y=syntax_error(undeclared(X)) }.

rest_expression(N,E1,E,V) -->
    arith_operator(N,Op),
    { N1 is N-1 },
    sub_expression(N1,E2,V),
    rest_expression(N,expr(Op,E1,E2),E,V).
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

declared(X,V) :- member(X,V).

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

preface --> [begin], !.
preface --> [_], !, preface.

epilogue(Es) --> [.], !, {Es=[]}, anything.
epilogue(Es) --> [X], !, {Es=[X|Xs]}, something(Xs).
epilogue([]) --> [].

anything --> [_], !, anything.
anything --> [].

something(Es) --> [X], !, {Es=[X|Xs]},  something(Xs).
something([]) --> [].

declaration_error([;]) --> [;], !.
declaration_error([begin]), [begin] --> [begin], !.
declaration_error([end]), [end] --> [end], !.
declaration_error([if]), [if] --> [if], !.
declaration_error([while]), [while] --> [while], !.
declaration_error([X|Xs]) --> [X], !, declaration_error(Xs).
declaration_error([]) --> [].

statement_error([X|Xs]) --> [X], !, statement_error_(Xs).
statement_error([]) --> [].

statement_error_([X]), [X] --> [X], {end_of_statement(X)}, !.
statement_error_([X|Xs]) --> [X], !, statement_error_(Xs).
statement_error_([]) --> [].

test_error([X]), [X] --> [X], {end_of_test(X)}, !.
test_error([X|Xs]) --> [X], !, test_error(Xs).
test_error([]) --> [].

expression_error([X]), [X] --> [X], {end_of_expression(X)}, !.
expression_error([X|Xs]) --> [X], !, expression_error(Xs).
expression_error([]) --> [].

syntax_error([X]) --> [X], {end_of_statement(X)}, !.
syntax_error([X|Xs]) --> [X], !, syntax_error(Xs).
syntax_error([]) --> [].

contains_syntax_error(Structure) :-
    functor(Structure,F,N),
    ( F==syntax_error -> true
    ; syntax_error_in_args(Structure,N)
    ).

syntax_error_in_args(Structure,N) :-
    N>0, arg(N,Structure,A),
    ( contains_syntax_error(A) -> true
    ; N1 is N-1, syntax_error_in_args(Structure,N1)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
