with Ada.Exceptions; use Ada.Exceptions;
generic
	     type Elem is private;
	     type Index is (<>);
	     type Tömb is array (Index range <>) of Elem;
	     with function Feltétel ( A: Elem ) return Boolean;
	     with function "<" ( A, B: Elem ) return Boolean is <>;
procedure Felt_Max_Ker ( T: in Tömb; Van: out Boolean; Max: out Elem );

