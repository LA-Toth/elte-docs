with Verem_DG, Text_IO;
use Text_IO;

procedure Verem_DG_Proba is
   package Int_Verem is new Verem_DG ( Integer );

   use Int_Verem;

   V : Verem;
begin
   Betev�s ( V, 1 );
   Betev�s ( V, 2 );
   Kidob�s ( V );
   Put_Line ( Integer'Image ( Tet� ( V )));
end Verem_DG_Proba;