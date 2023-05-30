function Faktorialis( N: Natural ) return Positive is
   Fakt : Positive := 1;
begin
   for I in 1..N loop
      Fakt := Fakt * I;
   end loop;
   return Fakt;
end Faktorialis;
