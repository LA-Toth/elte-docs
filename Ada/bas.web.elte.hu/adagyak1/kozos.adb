procedure Kozos ( A, B : in Positive; LNKO, LKKT : out Positive ) is
   X : Positive := A;
   Y : Positive := B;
begin
   while X /= Y loop
      if X > Y then
         X := X - Y;
      else
         Y := Y - X;
      end if;
   end loop;
   LNKO := X;
   LKKT := A * B / LNKO;
end Kozos;