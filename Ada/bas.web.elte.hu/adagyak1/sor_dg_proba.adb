with Sor_DG, Text_IO;
use Text_IO;

procedure Sor_DG_Proba is
   package Int_Sor is new Sor_DG ( Integer );
   
   use Int_Sor;
   
   S : Sor;
begin
   Betev�s ( S, 1 );
   Betev�s ( S, 2 );
   Kidob�s ( S );
   Put_Line ( Integer'Image ( Els� ( S )));
end Sor_DG_Proba;