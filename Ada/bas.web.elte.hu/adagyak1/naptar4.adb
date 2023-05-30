with Text_IO;
use Text_IO;

procedure Naptar4 is

   type Nap_Típus is ( Hétfõ, Kedd, Szerda, Csütörtök,
                       Péntek, Szombat, Vasárnap );

   Nap : Nap_Típus;
   Szám : String ( 1 .. 9 );
   Szám_Hossza : Natural;

begin
   Put ( "Nap: " );
   Get_Line ( Szám, Szám_Hossza );
   Nap := Nap_Típus'Value ( Szám ( 1 .. Szám_Hossza ));
   case Nap is
      when Hétfõ => Put_Line ( "Elsõ munkanap." );
      when Szerda => Put_Line ( "Középsõ munkanap." );
      when Péntek => Put_Line ( "Utolsó munkanap." );
      when Kedd | Csütörtök => Put_Line ( "Szürke hétköznap." );
      when Szombat .. Vasárnap => Put_Line ( "Hétvége." );
   end case;
end Naptar4;