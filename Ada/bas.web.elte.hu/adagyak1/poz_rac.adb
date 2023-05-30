package body Poz_Rac is
   function LNKO ( M, N : Positive ) return Positive is 
      A : Positive := M;
      B : Positive := N;
   begin
      while A /= B loop
         if A > B then
            A := A - B;
         else
            B := B - A;
         end if;
      end loop;
      return A;
   end LNKO;

   function Egyszer�s�tve ( P : Pozrac ) return Pozrac is
      Kozos : Positive := LNKO ( P.Sz�ml�l�, P.Nevez� );
   begin
      return ( P.Sz�ml�l� / Kozos, P.Nevez� / Kozos );
   end Egyszer�s�tve;

   function "/" ( Sz�ml�l�, Nevez� : Positive ) return Pozrac is
   begin
      return Egyszer�s�tve (( Sz�ml�l�, Nevez� ));
   end "/";

   function "/" ( P, Q : Pozrac ) return Pozrac is
   begin
      return Egyszer�s�tve (( P.Sz�ml�l� * Q.Nevez�, P.Nevez� * Q.Sz�ml�l� ));
   end "/";

   function "/" ( P : Pozrac; N : Positive ) return Pozrac is
   begin
      return P / Pozrac' ( N / 1 );
   end "/";

   function Sz�ml�l� ( P : Pozrac ) return Positive is
   begin
      return P.Sz�ml�l�;
   end;

   function Nevez� ( P : Pozrac ) return Positive is
   begin
      return P.Nevez�;
   end;
end Poz_Rac;