with Felt_Max_Ker;
with Ada.Integer_Text_IO, Ada.Float_Text_IO;
use Ada.Integer_Text_IO, Ada.Float_Text_IO;
procedure Max_Demo is
	type T is array (Integer range <>) of Float;
	function Egész ( A: Float ) return Boolean is
	begin
            return A = Float(Integer(A));
	end Egész;
	procedure Max is new Felt_Max_Ker(Float,Integer,T,Egész);
	Y: T(1..10) := (1.4,5.2,3.6,7.0,2.0,65.5,3.0,56.0,2.0,56.0);
	F: Float;
	V: Boolean;
begin
	Max(Y,V,F);
	if V then Put( F ); end if;
end;
