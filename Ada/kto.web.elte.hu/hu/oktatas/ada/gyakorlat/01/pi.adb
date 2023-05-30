with Ada.Float_Text_IO;
procedure Pi is
   Prod : Float := 1.0;
   Szamlalo : Float := 0.0;
begin
   for I in 1..2000 loop
      Szamlalo := Szamlalo + 2.0;
      Prod := Prod * (Szamlalo / (Szamlalo-1.0)) * (Szamlalo / (Szamlalo+1.0));
   end loop;
   Ada.Float_Text_IO.Put( 2.0 * Prod );
end Pi;
