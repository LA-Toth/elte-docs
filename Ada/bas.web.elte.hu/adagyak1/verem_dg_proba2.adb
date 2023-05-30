with Verem_DG, Text_IO;
use Text_IO;

procedure Verem_DG_Proba2 is
   package Int_Verem is new Verem_DG ( Integer );

   use Int_Verem;

   Mélység : String ( 1 .. 10 );
   Mélység_Hossz : Natural;
begin
   Put ( "A verem mérete: " );
   Get_Line ( Mélység, Mélység_Hossz );
   declare
      V : Verem ( Méret'Value ( Mélység ( 1 .. Mélység_Hossz )));
   begin
      Betevés ( V, 1 );
      Betevés ( V, 2 );
      Kidobás ( V );
      Put_Line ( Integer'Image ( Tetõ ( V )));
   end;
end Verem_DG_Proba2;