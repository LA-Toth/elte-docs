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
