function Fakt ( Number : Natural ) return Natural is
begin
   if N = 0 then
      return 1;
   else
      return Number * Fakt ( Number - 1 );
   end if;
end Fakt;