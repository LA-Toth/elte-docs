with Balinverz, Text_IO;
use Text_IO;

procedure Balinverz_Proba is
   type Felcsoport is new Integer range - 30000 .. 40000;

   procedure Bi is new Balinverz ( Felcsoport, 0, "+" );

   X, Y : Felcsoport;
   Van : Boolean;
   XS : String ( 1 .. 5 );
   XL : Natural;
begin
   New_Line;
   Put ( "Melyik szamnak szamoljam a balinverzet [" );
   Put ( Felcsoport'Image ( Felcsoport'First ));
   Put ( ".." );
   Put ( Felcsoport'Image ( Felcsoport'Last ));
   Put ( "]? " );
   Get_Line ( XS, XL );
   X := Felcsoport'Value ( XS ( 1 .. XL ));
   Bi ( X, Y, Van );
   if Van	then
      Put ( Felcsoport'Image ( Y ));
   else
      Put ( "nincs" );
   end if;
   New_Line ( 2 );
end Balinverz_Proba;