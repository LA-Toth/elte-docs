with Ada.Integer_Text_IO;
with Faktorialis;
use Ada.Integer_Text_IO;
procedure Faktszamol is
   N : Natural;
begin
   Get( N );
   Put( Faktorialis(N) );
end Faktszamol;
