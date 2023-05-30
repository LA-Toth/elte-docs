with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Cserel, Cserel_Trukkos;
procedure Cserel_Demo is
   A, B : Integer;
begin
   Get( A );
   Get( B );
   Cserel( A, B );
   Put( A ); Put( B );
   Cserel_Trukkos( A, B );
   Put( A ); Put( B );
end Cserel_Demo;
