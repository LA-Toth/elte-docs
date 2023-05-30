with Ada.Finalization; use Ada.Finalization;
generic
		type Elem is private;
package Sorok is
		type Sor is new Limited_Controlled with private;
		procedure Finalize ( S: in out Sor );
		procedure Betesz ( S: in out Sor; E: in Elem );
		procedure Kivesz ( S: in out Sor; E: out Elem );
		Üres_Sor: exception;
		generic
			with procedure Feladat ( E: in Elem );
		procedure Iterál ( S: in Sor );
private
		type Csúcs;
		type Mutató is access Csúcs;
		type Csúcs is record
				   Adat: Elem;
				   Következõ: Mutató;
			          end record;
		type Sor is new Limited_Controlled with record
				Eleje, Vége: Mutató := null;
			      end record;
end Sorok;

