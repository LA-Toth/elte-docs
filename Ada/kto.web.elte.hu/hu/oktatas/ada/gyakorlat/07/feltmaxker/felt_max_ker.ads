with Ada.Exceptions; use Ada.Exceptions;
generic
	     type Elem is private;
	     type Index is (<>);
	     type T�mb is array (Index range <>) of Elem;
	     with function Felt�tel ( A: Elem ) return Boolean;
	     with function "<" ( A, B: Elem ) return Boolean is <>;
procedure Felt_Max_Ker ( T: in T�mb; Van: out Boolean; Max: out Elem );

