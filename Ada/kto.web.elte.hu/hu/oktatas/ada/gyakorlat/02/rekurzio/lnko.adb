function Lnko ( A, B : Positive ) return Positive is
begin
   if A > B then return Lnko(A-B,B);
   elsif A < B then return Lnko(A,B-A);
   else return A;
   end if;
end Lnko;
