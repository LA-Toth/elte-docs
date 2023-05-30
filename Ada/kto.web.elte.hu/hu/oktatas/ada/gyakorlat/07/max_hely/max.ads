with Ada.Exceptions; use Ada.Exceptions;
generic
	     type Elem is limited private;
	     type Index is (<>);
	     type Tömb is array (Index range <>) of Elem;
	     with function "<" ( A, B: Elem ) return Boolean is <>;
	     Üres_Tömb: Exception_Id := Constraint_Error ' Identity;
function Max ( T: Tömb ) return Elem;

