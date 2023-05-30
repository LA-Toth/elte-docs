with Max, Max_Hely;
with Ada.Integer_Text_IO, Ada.Float_Text_IO;
use Ada.Integer_Text_IO, Ada.Float_Text_IO;
procedure Max_Demo is
		type T is array (Integer range <>) of Float;
		Baki: exception;
		function Float_Max is new Max(Float,Integer,T);
		function Float_Min is new Max( Float, Integer, T, ">",
							Baki ' Identity );
		function Float_Max_H is new Max_Hely(Float,Integer,T);
		function Float_Min_H is new Max_Hely( Float, Integer, T, ">",
							Baki ' Identity );
		X: T(1..0);    -- üres tömb
		Y: T(1..10) := (1.0,5.0,3.0,7.0,2.0,65.0,3.0,56.0,3.0,56.0);
begin
		Put( Float_Max_H(Y) );
		Put( Float_Max(Y) );
		Put( Float_Min_H(Y) );
		Put( Float_Min(Y) );
		--Put( Float_Max_H(X) );
		--Put( Float_Max(X) );
		--Put( Float_Min_H(X) );
		Put( Float_Min(X) );
end;
