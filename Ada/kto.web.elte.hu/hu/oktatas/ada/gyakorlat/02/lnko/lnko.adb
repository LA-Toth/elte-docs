function Lnko ( A, B : Positive ) return Positive is
   X: Positive := A;
   Y: Positive := B;
begin
   while X /= Y loop
      if X > Y then
         X := X - Y;
      else
         Y := Y - X;
      end if;
   end loop;
   return X;
end Lnko;
