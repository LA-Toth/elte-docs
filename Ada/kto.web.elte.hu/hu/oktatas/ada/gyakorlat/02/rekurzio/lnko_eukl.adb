function Lnko_Eukl ( A, B : Positive ) return Positive is
   function Lnko_Rek ( A, B : Natural ) return Positive is
   begin
      if B = 0 then
         return A;
      else
         return Lnko_Rek( B, A mod B );
      end if;
   end Lnko_Rek;
begin
   return Lnko_Rek( A, B );
end Lnko_Eukl;
