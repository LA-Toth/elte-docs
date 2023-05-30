with Sor_D, Text_IO;
use Sor_D, Text_IO;

procedure Sor_D_Proba is
   S : Sor;
begin
   Betevés ( S, 1 );
   Betevés ( S, 2 );
   Kidobás ( S );
   Put_Line ( Integer'Image ( Elsõ ( S )));
end Sor_DG_Proba;