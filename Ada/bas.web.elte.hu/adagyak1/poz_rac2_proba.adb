with Poz_Rac2, Text_IO;
use Poz_Rac2, Text_IO;

procedure Poz_Rac2_Proba is 
   P : Pozrac := 8 / 6;
   Q : Pozrac := 2 / 3;

   procedure Kiir ( P : in Pozrac ) is
   begin
      Put ( Positive'Image ( Sz�ml�l� ( P )) & '/' &
            Positive'Image ( Nevez� ( P )));
   end Kiir;
begin
   P := P / 2;
   Kiir ( P );
      P := P * Q;
   Kiir ( P );
      P := P + Q;
   Kiir ( P );
   New_Line;
end Poz_Rac2_Proba;
                