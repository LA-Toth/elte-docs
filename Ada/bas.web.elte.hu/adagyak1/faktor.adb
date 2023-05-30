with Text_IO;
use Text_IO;

procedure Faktor is
   F : Natural := 1;
begin
   for I in 1 .. 10 loop
      F := F * I;
   end loop;
   Put_Line ( "10! = " & Natural'Image ( F ));
end Faktor;