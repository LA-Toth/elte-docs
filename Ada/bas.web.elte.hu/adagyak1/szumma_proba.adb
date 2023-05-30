with Szumma, Text_IO;
use Text_IO;

procedure Szumma_Proba is
   type Int_Vektor is array ( Integer range <> ) of Integer;

   function Int_Prod is new Szumma ( Integer, Integer, Int_Vektor, 1, "*" );
begin
   Put_Line ( Integer'Image ( Int_Prod (( 1, 2, 3, 4 ))));
end Szumma_Proba;