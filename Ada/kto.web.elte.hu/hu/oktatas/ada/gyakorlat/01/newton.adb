with Ada.Float_Text_IO;
procedure Newton is
   A : Float;
   X : Float := 1.0;
   Diff: constant Float := 0.01;
   Regi_X : Float;
begin
   Ada.Float_Text_IO.Get( A );

--   Regi_X := A;
--   while abs (X-Regi_X) >= Diff loop
--      Regi_X := X;
--      X := 0.5 * (X + A/X);
--   end loop;

   loop
      Regi_X := X;
      X := 0.5 * (X + A/X);
      exit when abs (X-Regi_X) < Diff;
   end loop;

   Ada.Float_Text_IO.Put( X );
end Newton;
