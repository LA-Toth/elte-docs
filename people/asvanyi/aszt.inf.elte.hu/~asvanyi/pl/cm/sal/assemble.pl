:- module( assemble, [assemble/3] ).

assemble(Code,Dictionary,TidyCode) :-
    abs_code(Code,Dictionary,TidyCode,[]).

abs_code(Code,D) -->
    tidy_code(Code,0,N), [s(N,halt,0),s(N1,block,L)],
    {N1 is N+1, allocate(D,N1,N2), L is N2-N1}.

tidy_code([instr(subc,0)|Code],M,N) --> !,
    tidy_code(Code,M,N).
tidy_code([instr(Statement,Operandus)|Code],M,N) --> !,
    [s(M,Statement,Operandus)], {M1 is M+1},
    tidy_code(Code,M1,N).
tidy_code([label(M)|Code],M,N) --> % !,
    tidy_code(Code,M,N).
tidy_code([],N,N) --> [].

allocate(void,M,N) :- !, N=M.
allocate(dict(_,M1,Before,After),M,N) :-
    allocate(Before,M,M1),
    M2 is M1+1,
    allocate(After,M2,N).

