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

   function Egyszerûsítve ( P : Pozrac ) return Pozrac is
      Kozos : Positive := LNKO ( P.Számláló, P.Nevezõ );
   begin
      return ( P.Számláló / Kozos, P.Nevezõ / Kozos );
   end Egyszerûsítve;

   function "/" ( Számláló, Nevezõ : Positive ) return Pozrac is
   begin
      return Egyszerûsítve (( Számláló, Nevezõ ));
   end "/";

   function "/" ( P, Q : Pozrac ) return Pozrac is
   begin
      return Egyszerûsítve (( P.Számláló * Q.Nevezõ, P.Nevezõ * Q.Számláló ));
   end "/";

   function "/" ( P : Pozrac; N : Positive ) return Pozrac is
   begin
      return P / Pozrac' ( N / 1 );
   end "/";

   function Számláló ( P : Pozrac ) return Positive is
   begin
      return P.Számláló;
   end;

   function Nevezõ ( P : Pozrac ) return Positive is
   begin
      return P.Nevezõ;
   end;
end Poz_Rac;