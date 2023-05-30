with Text_IO;
use Text_IO;

procedure Naptar2 is

   subtype Nap_Típus is Positive range 1 .. 7;

   Nap : Nap_Típus;
   Szám : String ( 1 .. 10 );
   Szám_Hossza : Natural;

begin
   Put ( "Nap: " );
   Get_Line ( Szám, Szám_Hossza );
   Nap := Positive'Value ( Szám ( 1 .. Szám_Hossza ));
   case Nap is
      when 1 => Put_Line ( "Elsõ munkanap." );
      when 3 => Put_Line ( "Középsõ munkanap." );
      when 5 => Put_Line ( "Utolsó munkanap." );
      when 2 | 4 => Put_Line ( "Szürke hétköznap." );
      when 6 .. 7 => Put_Line ( "Hétvége." );
   end case;
end Naptar2;