with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Mat; -- use Mat;
procedure Lnkozas is
   A, B : Positive;
begin
   Get( A );
   Get( B );
   Put( Mat.Lnko(A,B) );
end Lnkozas;
