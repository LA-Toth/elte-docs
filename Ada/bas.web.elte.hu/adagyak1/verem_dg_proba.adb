with Verem_DG, Text_IO;
use Text_IO;

procedure Verem_DG_Proba is
   package Int_Verem is new Verem_DG ( Integer );

   use Int_Verem;

   V : Verem;
begin
   Betevés ( V, 1 );
   Betevés ( V, 2 );
   Kidobás ( V );
   Put_Line ( Integer'Image ( Tetõ ( V )));
end Verem_DG_Proba;