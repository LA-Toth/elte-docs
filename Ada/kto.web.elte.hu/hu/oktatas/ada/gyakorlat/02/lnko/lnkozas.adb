with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Lnko, Lnko_Eukl;
procedure Lnkozas is
   A, B : Positive;
begin
   Get( A );
   Get( B );
   Put( Lnko(A,B) );
   Put( Lnko_Eukl(A,B) );
end Lnkozas;
