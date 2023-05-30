procedure Masodfokut_megold ( 
   A, B, C : in Float;    -- Legalább az egyik legyen nemnulla
   Megoldasok_Szama: out Natural;
   X1, X2: out Float        )
is

   Diszkriminans: Float := B**2 - 4.0*A*C;
   Negyzetgyok_D: Float;

   function Negyzetgyok( A: Float ) return Float is
      X : Float := 1.0;
      Diff: constant Float := 0.00001;
      Regi_X : Float;
   begin
      loop
         Regi_X := X;
         X := 0.5 * (X + A/X);
         exit when abs (X-Regi_X) < Diff;
      end loop;
      return X;
   end Negyzetgyok;

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
      Negyzetgyok_D := Negyzetgyok(Diszkriminans);
      X1 := (-B + Negyzetgyok_D) / (2.0 * A);
      X2 := (-B - Negyzetgyok_D) / (2.0 * A);
   end if;
end Masodfokut_Megold;
