with Ada.Exceptions; use Ada.Exceptions;
generic
	     type Elem is limited private;
	     type Index is (<>);
	     type T�mb is array (Index range <>) of Elem;
	     with function "<" ( A, B: Elem ) return Boolean is <>;
	     �res_T�mb: Exception_Id := Constraint_Error ' Identity;
function Max ( T: T�mb ) return Elem;

