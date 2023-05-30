with Text_IO;
use Text_IO;

procedure Naptar2 is

   subtype Nap_T�pus is Positive range 1 .. 7;

   Nap : Nap_T�pus;
   Sz�m : String ( 1 .. 10 );
   Sz�m_Hossza : Natural;

begin
   Put ( "Nap: " );
   Get_Line ( Sz�m, Sz�m_Hossza );
   Nap := Positive'Value ( Sz�m ( 1 .. Sz�m_Hossza ));
   case Nap is
      when 1 => Put_Line ( "Els� munkanap." );
      when 3 => Put_Line ( "K�z�ps� munkanap." );
      when 5 => Put_Line ( "Utols� munkanap." );
      when 2 | 4 => Put_Line ( "Sz�rke h�tk�znap." );
      when 6 .. 7 => Put_Line ( "H�tv�ge." );
   end case;
end Naptar2;