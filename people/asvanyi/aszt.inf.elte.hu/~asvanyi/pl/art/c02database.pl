:- use_module(c01basic).

	uncle(Uncle,Person) :-
		brother(Uncle,Parent), parent(Parent,Person).

	sibling(Sib1,Sib2) :-
		parent(Parent,Sib1), parent(Parent,Sib2), Sib1 \== Sib2.

	cousin(Cousin1,Cousin2) :-
		parent(Parent1,Cousin1),
		parent(Parent2,Cousin2),
		sibling(Parent1,Parent2).

%	Program 2.1:  Defining family relationships


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	resistor(power,n1).
	resistor(power,n2).

	transistor(n2,ground,n1).
	transistor(n3,n4,n2).
	transistor(n5,ground,n4).

/*
	inverter(Input,Output) :-
		Output is the inversion of Input.
*/

	inverter(Input,Output) :-
		transistor(Input,ground,Output),
		resistor(power,Output).

/*
	nand_gate(Input1,Input2,Output):-
	   Output is the logical nand of Input1 and Input2.
*/

	nand_gate(Input1,Input2,Output) :-
		transistor(Input1,X,Output),
		transistor(Input2,ground,X),
		resistor(power,Output).

/*
	and_gate(Input1,Input2,Output):-
	  Output is the logical and of Input1 and Input2.		     
*/

	and_gate(Input1,Input2,Output) :-
		nand_gate(Input1,Input2,X),
		inverter(X,Output).

%	Program 2.2: A circuit for a logical and-gate

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
	resistor(R,Node1,Node2) :-
	   R is a resistor between Node1 and Node2.
*/
	resistor(r1,power,n1).
	resistor(r2,power,n2).

/*
	transistor(T,Gate,Source,Drain) :-
		T is a transistor whose gate is Gate,
		source is Source, and drain is Drain.
*/

	transistor(t1,n2,ground,n1).
	transistor(t2,n3,n4,n2).
	transistor(t3,n5,ground,n4).

/*
	inverter(I,Input,Output) :-
	  I is an inverter which inverts Input to Output.
*/
	inverter(inv(T,R),Input,Output) :-
		transistor(T,Input,ground,Output),
		resistor(R,power,Output).

/*
	nand_gate(Nand,Input1,Input2,Output):-
		Nand is a gate forming the logical nand, Output,
		of Input1 and Input2.
*/
	nand_gate(nand(T1,T2,R),Input1,Input2,Output) :-
		transistor(T1,Input1,X,Output),
		transistor(T2,Input2,ground,X),
		resistor(R,power,Output).

/*
	and_gate(And,Input1,Input2,Output):-
		And is a gate forming the logical and, Output, 
		of Input1 and Input2.
*/

	and_gate(and(N,I),Input1,Input2,Output) :-
		nand_gate(N,Input1,Input2,X),
		inverter(I,X,Output).

%	   Program 2.3: The circuit database with names

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	lecturer(Lecturer,Course) :-
%		course(Course,Time,Lecturer,Location).

	lecturer(Lecturer,Course) :-
		course(Course,_,Lecturer,_).

%	duration(Course,Length) :-
%		course(Course,time(Day,Start,Finish),Lecturer,Location),
%		plus(Start,Length,Finish).

	duration(Course,Length) :-
		course(Course,time(_,Start,Finish),_,_),
		Length is Finish - Start.

%	teaches(Lecturer,Day) :-
%		course(Course,time(Day,Start,Finish),Lecturer,Location).

	teaches(Lecturer,Day) :-
		course(_,time(Day,_,_),Lecturer,_).

%	occupied(Room,Day,Time) :-
%		course(Course,time(Day,Start,Finish),Lecturer,Location),
%		Start =< Time, Time =< Finish.

	occupied(Room,Day,Time) :-
		course(_,time(Day,Start,Finish),_,Room),
		Start =< Time, Time =< Finish.

%	Program 2.4: Course rules

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
	ancestor_of(Ancestor,Descendant)  :-
		Ancestor is an ancestor of Descendant.
*/
	ancestor_of(Ancestor,Descendant) :-
		parent(Ancestor,Descendant).
	ancestor_of(Ancestor,Descendant) :-
		parent(Ancestor,Person), ancestor_of(Person,Descendant).

%	Program 2.5: The ancestor relationship

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	edge(a,b).	edge(a,c).	edge(b,d).
	edge(c,d).	edge(d,e).	edge(f,g).

%	Program 2.6: A directed graph

/*
	connected(Node1,Node2) :-
		Node1 is connected to Node2 in the graph
		defined by the edge/2 relation.
*/
	connected(Node,Node).
	connected(Node1,Node2) :- edge(Node1,Link), connected(Link,Node2).

%	Program 2.7: The transitive closure of the edge relationship

