with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
with Masodfokut_Megold;
procedure Masodfoku is
   A, B, C : Float;
   N: Natural;
begin
   Get( A );
   Get( B );
   Get( C );
   Masodfokut_Megold( A, B, C, N, A, B );
   case N is
      when 1 => Put(A);
      when 2 => Put(A); Put(B);
      when others => null;
   end case;
end Masodfoku;
