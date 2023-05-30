with Ada.Finalization; use Ada.Finalization;
generic
		type Elem is private;
package Sorok is
		type Sor is new Limited_Controlled with private;
		procedure Finalize ( S: in out Sor );
		procedure Betesz ( S: in out Sor; E: in Elem );
		procedure Kivesz ( S: in out Sor; E: out Elem );
		�res_Sor: exception;
		generic
			with procedure Feladat ( E: in Elem );
		procedure Iter�l ( S: in Sor );
private
		type Cs�cs;
		type Mutat� is access Cs�cs;
		type Cs�cs is record
				   Adat: Elem;
				   K�vetkez�: Mutat�;
			          end record;
		type Sor is new Limited_Controlled with record
				Eleje, V�ge: Mutat� := null;
			      end record;
end Sorok;

