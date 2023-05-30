with Ada.Integer_Text_IO;
procedure Faktor is
   N : Integer;
   Fakt : Integer := 1;
begin
   Ada.Integer_Text_IO.Get( N );
   for I in 1..N loop
      Fakt := Fakt * I;
   end loop;
   Ada.Integer_Text_IO.Put( Fakt );
end Faktor;
