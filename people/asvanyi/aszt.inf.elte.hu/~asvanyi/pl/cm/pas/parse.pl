% Parsing a tokenlist of a Simple Pascal program.
% Discovering a syntax error using default rules.

:- module( parse, [parse/2] ).

parse(Tokens,Structure) :-
    bb_put(syntax_error,false),
    program(Structure,Tokens,RemTokens),
    ( bb_get(syntax_error,true) -> throw('SYNTAX ERROR(S)')
    ; RemTokens \== [] -> throw('COMPILATION ERROR(S)')
    ; true
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program(S) -->
    program_head, program_body(S), !, program_end.

program_head -->
    [program], identifier(_), [;], !. 
program_head -->
    head_error(L),{error(L,'Error in head of program.')}.

program_end --> [.], !, anything(_).
program_end -->
    anything([X|Xs]), !, {error([X|Xs],
				'Garbage instead of end of program.')}.
program_end --> anything([]).
	
program_body(Ss) --> [int], !, declarations(Vs), statement_part(Ss,Vs).
program_body(Ss) --> statement_part(Ss,[]).

declarations([V|Vs]) -->
    variable(V), rest_variables(Vs), !.
declarations(_) -->
    declaration_error(L), {error(L,'Non-variable on declaration list.')}.

rest_variables([V|Vs]) --> [','], variable(V), rest_variables(Vs).
rest_variables([]) --> [;].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ss is the list representing the syntax tree of the statement part.

statement_part(Ss,V) -->
    [begin], !, statement_list(Ss,V).
statement_part([syntax_error],_) --> 
    syntax_error(L),{error(L,'Keyword "begin" misspelled.')}.

statement_list([],_V) --> [end], !.
statement_list([S|Ss],V) --> statement(S,V), !, rest_statements(Ss,V).

rest_statements(Ss,V) --> [;], !, statement_list(Ss,V).
rest_statements([],_) --> [end], !.
rest_statements([syntax_error],_) -->
    rest_error(L),
    {error(L,'Error in statement list: ";" or "end" was waited.')}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statement(Ss,V) -->
    [begin], !, statement_list(Ss,V).
statement(assign(X,E),V) --> 
    variable(X), [:=], {declared(X,V)}, !, expression(E,V).
statement(if(T,S1,S2),V) -->
    [if], test(T,V), [then], statement(S1,V), [else], !, statement(S2,V).
statement(while(T,Ss),V)  --> 
    [while], test(T,V), [do], !, statement(Ss,V).
statement(read(X),V)     -->
    [read], variable(X), {declared(X,V)}, !.
statement(write(X),V)    -->
    [write], expression(X,V), !. 
statement([],_), [X] -->         % empty statetement
    [X], {end_of_statement(X)}, !.
statement(syntax_error,_) --> 
    statement_error(L),{error(L,'Wrong statement.')}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(compare(Op,X,Y),V) -->
    expression(X,V), comparison_op(Op), expression(Y,V), test_end, !.
test(syntax_error,_V) -->
    test_error(L), {error(L,'Wrong test.')}.

expression(Expr,V) -->
    sub_expression(2,Expr,V), expression_end, !.
expression(syntax_error,_V) -->
    expression_error(L), {error(L,'Wrong expression.')}.

sub_expression(N,E,V) -->
    { N>0, N1 is N-1 },
    sub_expression(N1,E1,V), rest_expression(N,E1,E,V).
sub_expression(0,E,V) -->
    ['('], sub_expression(2,E,V), [')'] ; atomic_expression(E,V).

atomic_expression(number(X),_) --> constant(X).
atomic_expression(name(X),V) --> variable(X), {declared(X,V)}.

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

declared(X,V) :-
    ( member(X,V) -> true
    ; atom_concat(X,' is not declared.',Y),
      error([Y],'Undefined variable.')
    ).

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

head_error([;]) --> [;], !.
head_error([]), [begin] --> [begin], !.
head_error([]), [int] --> [int], !.
head_error([X|Xs]) --> [X], !, head_error(Xs).
head_error([]) --> [].

anything([X|Xs]) --> [X], !, anything(Xs).
anything([]) --> [].

declaration_error([;]) --> [;], !.
declaration_error([]), [begin] --> [begin], !.
declaration_error([X|Xs]) --> [X], !, declaration_error(Xs).
declaration_error([]) --> [].

rest_error(L) --> statement_error(L), rest_of_block.

rest_of_block --> [end], !.
rest_of_block --> [_], !, rest_of_block.
rest_of_block --> [].

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

error(L,Message) :-
    nl(user), show_error(L,40), 
    write(user,Message), nl(user),
    bb_put(syntax_error,true).

show_error([],_) :-  nl(user).
show_error([X|Xs],N) :-
    ( N > 0 ->
      N1 is N-1, write(user,' '), write(user,X),
      show_error(Xs,N1)
    ; nl(user)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
