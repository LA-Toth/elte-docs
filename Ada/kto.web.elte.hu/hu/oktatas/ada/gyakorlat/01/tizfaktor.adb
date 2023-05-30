with Ada.Integer_Text_IO;
procedure Tizfaktor is
   Fakt: Integer := 1;
begin
   for I in 1..10 loop
      Fakt := Fakt * I;
   end loop;
   Ada.Integer_Text_IO.Put(Fakt);
end Tizfaktor;
