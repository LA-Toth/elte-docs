with Verem_DG, Text_IO;
use Text_IO;

procedure Verem_DG_Proba2 is
   package Int_Verem is new Verem_DG ( Integer );

   use Int_Verem;

   M�lys�g : String ( 1 .. 10 );
   M�lys�g_Hossz : Natural;
begin
   Put ( "A verem m�rete: " );
   Get_Line ( M�lys�g, M�lys�g_Hossz );
   declare
      V : Verem ( M�ret'Value ( M�lys�g ( 1 .. M�lys�g_Hossz )));
   begin
      Betev�s ( V, 1 );
      Betev�s ( V, 2 );
      Kidob�s ( V );
      Put_Line ( Integer'Image ( Tet� ( V )));
   end;
end Verem_DG_Proba2;