with Sor_DG, Text_IO;
use Text_IO;

procedure Sor_DG_Proba is
   package Int_Sor is new Sor_DG ( Integer );
   
   use Int_Sor;
   
   S : Sor;
begin
   Betevés ( S, 1 );
   Betevés ( S, 2 );
   Kidobás ( S );
   Put_Line ( Integer'Image ( Elsõ ( S )));
end Sor_DG_Proba;