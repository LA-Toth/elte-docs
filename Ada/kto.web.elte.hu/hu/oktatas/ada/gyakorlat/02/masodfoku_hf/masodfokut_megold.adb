with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
procedure Masodfokut_megold ( 
   A, B, C : in Float;    -- Legalább az egyik legyen nemnulla
   Megoldasok_Szama: out Natural;
   X1, X2: out Float        )
is
   Diszkriminans: Float := B**2 - 4.0*A*C;
   Negyzetgyok_D: Float;
begin
   if A = 0.0 then
      if B = 0.0 then
         Megoldasok_Szama := 0;
      else
         Megoldasok_Szama := 1;
         X1 := -C/B;
         X2 := X1;
      end if;
   elsif Diszkriminans < 0.0 then
      Megoldasok_Szama := 0;
   elsif Diszkriminans = 0.0 then
      Megoldasok_Szama := 1;
      X1 := (-B) / (2.0 * A);
      X2 := X1;
   else
      Megoldasok_Szama := 2;
      Negyzetgyok_D := Sqrt(Diszkriminans);
      X1 := (-B + Negyzetgyok_D) / (2.0 * A);
      X2 := (-B - Negyzetgyok_D) / (2.0 * A);
   end if;
end Masodfokut_Megold;
