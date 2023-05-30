:- module( encode, [encode/3] ).

encode(Structure,Dictionary,Code) :-
    code(Structure,Dictionary,Code,[]).

code([S|Ss],D) --> code(S,D), code(Ss,D).
code([],_) --> [].

code(assign(X,E),D) -->
    expr_code(E,D,1), {lookup(X,D,Address)}, [instr(store,Address)].
code(if(Test,Then,Else),D) -->
    test_code(Test,L1,D), code(Then,D), [instr(jump,L2)],
    [label(L1)], code(Else,D), [label(L2)].
code(while(Test,Do),D) -->
    [label(L1)], test_code(Test,L2,D), code(Do,D),
    [instr(jump,L1), label(L2)].
code(read(X),D) -->
    {lookup(X,D,Address)}, [instr(read,Address)].
code(write(Expr),D) -->
    expr_code(Expr,D,1), [instr(write,0)].

expr_code(number(C),_,_) -->
    [instr(loadc,C)].
expr_code(name(X),D,_) -->
    [instr(load,Address)], {lookup(X,D,Address)}.
expr_code(expr(Op,E1,E2),D,I) -->
    { atomic_expr(E2), ! }, expr_code(E1,D,I), single_operation(E2,Op,D).
expr_code(expr(Op,E1,E2),D,I) -->
    { commutative(Op), atomic_expr(E1), ! },
    expr_code(E2,D,I), single_operation(E1,Op,D).
expr_code(expr(Op,E1,E2),D,I) -->    % General case
    % { \+ atomic_expr(E2), \+ (commutative(Op), atomic_expr(E1)) , ! },
    expr_code(E2,D,I), {lookup(I,D,Address)}, [instr(store,Address)],
    {I1 is I+1}, expr_code(E1,D,I1), 
    {memory_operation(Op,OpCode)}, [instr(OpCode,Address)].

single_operation(name(X),Op,D) -->
    {lookup(X,D,Address), memory_operation(Op,OpCode)},[instr(OpCode,Address)].
single_operation(number(C),Op,_) -->
    {literal_operation(Op,OpCode)}, [instr(OpCode,C)].

test_code(compare(Op,E1,E2),Label,D) -->
    expr_code(expr(-,E1,E2),D,1), 
    {comparison_opcode(Op,Cond_jump)}, [instr(Cond_jump,Label)].

lookup(Key,dict(Key1,Val,Left,Right),Value) :-
    ( Key = Key1 -> Val=Value
    ; Key @< Key1 -> lookup(Key,Left,Value)
    ; % Key @> Key1 ->
      lookup(Key,Right,Value)
    ).

atomic_expr(name(_)).    
atomic_expr(number(_)).

commutative(+).    
commutative(*).

memory_operation(+,add).    
memory_operation(-,sub).
memory_operation(*,mul).    
memory_operation(/,div).

literal_operation(+,addc).    
literal_operation(-,subc).
literal_operation(*,mulc).    
literal_operation(/,divc).

comparison_opcode(=,jumpne).     
comparison_opcode(\=,jumpeq).   
comparison_opcode(>,jumple).     
comparison_opcode(<,jumpge).
comparison_opcode(>=,jumplt).    
comparison_opcode(=<,jumpgt).
