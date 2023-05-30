
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%                               %%%%%%%%%%
%%%%                                               %%%%
% Prolog runtime environment for abstract programs... %
%%%%                                               %%%%
  %%%%%%%%%%                               %%%%%%%%%%
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(execute, [execute/1,run_program/1]).

:- bb_put(acc,0).

execute(Code) :-
	fill_blackboard(Code),
	Code = [s(Start,_,_)|_],
	run_program(Start).

fill_blackboard([]).
fill_blackboard([ Line | Code ]) :-
	Line = s(Addr,Stm,Op),
	( Stm == block -> End is Addr + Op, fill_data_area(Addr,End)
	; bb_put(Addr,s(Stm,Op))
	),
	fill_blackboard(Code).

fill_data_area(Addr,End) :-
    ( Addr < End ->
          bb_put(Addr,data(0)),
          Addr1 is Addr + 1,
          fill_data_area(Addr1,End)
    ; true
    ).

run_program(PC) :-
	( bb_get(PC,s(Statement,Operand)),
	  exec(Statement,Operand,Continue) ->
	  ( var(Continue) -> Continue is PC+1
	  ; true
	  ),
	  run_program(Continue)
	; PC == halt -> true
	; write(user,'\nIllegal access.\n')
	).

exec(halt,_,halt).

%%%%%%%%%%%%%%
% Read/Write %
%%%%%%%%%%%%%%

exec(read,Address,_Continue) :-
	getval(Val),
	store(Val,Address).

exec(write,_Address,_Continue) :-
	bb_get(acc,Val),
	write(': '), write(Val), write('\n').

%%%%%%%%%%%%%%
% Load/Store %
%%%%%%%%%%%%%%

exec(store,Address,_Continue) :-
	bb_get(acc,Val),
	store(Val,Address).

exec(load,Address,_Continue) :-
	load(Address,Val),
	bb_put(acc,Val).

exec(loadc,Value,_Continue) :-
	bb_put(acc,Value).

%%%%%%%%%
% Jumps %
%%%%%%%%%

exec(jump,Address,Address).

exec(jumpne,Address,Continue) :-
	bb_get(acc,Val),
	( Val =\= 0 -> Continue = Address
	; true
	).

exec(jumpeq,Address,Continue) :-
	bb_get(acc,Val),
	( Val =:= 0 -> Continue = Address
	; true
	).

exec(jumplt,Address,Continue) :-
	bb_get(acc,Val),
	( Val < 0 -> Continue = Address
	; true
	).

exec(jumple,Address,Continue) :-
	bb_get(acc,Val),
	( Val =< 0 -> Continue = Address
	; true
	).

exec(jumpgt,Address,Continue) :-
	bb_get(acc,Val),
	( Val > 0 -> Continue = Address
	; true
	).

exec(jumpge,Address,Continue) :-
	bb_get(acc,Val),
	( Val >= 0 -> Continue = Address
	; true
	).



%%%%%%%%%%%%%%%
% arithmetics %
%%%%%%%%%%%%%%%

exec(add,Address,_Continue) :-
	bb_get(acc,Val),
	load(Address,Var),
	V is Val+Var,
	bb_put(acc,V).
	
exec(sub,Address,_Continue) :-
	bb_get(acc,Val),
	load(Address,Var),
	V is Val-Var,
	bb_put(acc,V).
	
exec(mul,Address,_Continue) :-
	bb_get(acc,Val),
	load(Address,Var),
	V is Val*Var,
	bb_put(acc,V).

exec(div,Address,Continue) :-
	load(Address,Var),
	( Var =\= 0 ->
	  bb_get(acc,Val),
	  V is Val//Var,
	  bb_put(acc,V)
	; write(user,'\nZero divisor!\n'),
	  Continue = halt
	).

exec(addc,Var,_Continue) :-
	bb_get(acc,Val),
	V is Val+Var,
	bb_put(acc,V).

exec('subc',Var,_Continue) :-
	bb_get(acc,Val),
	V is Val-Var,
	bb_put(acc,V).

exec('mulc',Var,_Continue) :-
	bb_get(acc,Val),
	V is Val*Var,
	bb_put(acc,V).

exec('divc',Var,Continue) :-
	( Var =\= 0 ->
	  bb_get(acc,Val),
	  V is Val//Var,
	  bb_put(acc,V)
	; write(user,'\nZero divisor!\n'),
	  Continue = halt
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getval(X) :-
	write(user,'Program requires input.\n'),
	repeat,
	    write(user,'Enter an integer number, please: '),
	    flush_output(user),
	    get_line(Cs), 
	    catch( number_codes(X,Cs),
		   error(_,_),
		   fail
		 ),
	    integer(X),
	!.			%; repeat.

get_line(Codes) :-
    get_code(user,C),
    ( C == 0'\n -> Codes = []
    ; Codes = [C|Cs], get_line(Cs)
    ).

store(Val,Address) :-
    bb_get(Address,data(_)),
    bb_put(Address,data(Val)).

load(Address,Val) :- 
    bb_get(Address,data(Val)).



