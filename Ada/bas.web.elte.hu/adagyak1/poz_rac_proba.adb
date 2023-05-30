with Poz_Rac, Text_IO;
use Poz_Rac, Text_IO;

procedure Poz_Rac_Proba is 
   P : Pozrac := 8 / 6;  

   procedure Kiir ( P : in Pozrac ) is
   begin
      Put ( Positive'Image ( Számláló ( P )) & '/' &
            Positive'Image ( Nevezõ ( P )));
   end Kiir;
begin
   P := P / 2;
   Kiir ( P );
   New_Line;
end Poz_Rac_Proba;
                