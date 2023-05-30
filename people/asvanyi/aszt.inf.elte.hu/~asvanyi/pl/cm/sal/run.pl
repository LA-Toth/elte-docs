:- module( run, [run/1,compile/2,execute/1,run_program/1] ).

:- use_module( scan, [ scan/2 ] ).
:- use_module( parse, [parse /2 ] ).
:- use_module( encode, [ encode/3 ] ).
:- use_module( assemble, [ assemble/3 ] ).
:- use_module( execute, [ execute/1, run_program/1 ] ).

run(Prog) :- compile(Prog,ObjectCode), execute(ObjectCode).

compile(Prog,ObjectCode) :-
	atom_concat(Prog,'.sal',Name),
	scan(Name,Tokens),
	parse(Tokens,Structure),
	encode(Structure,Dictionary,Code),
	assemble(Code,Dictionary,ObjectCode).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(200)]).

