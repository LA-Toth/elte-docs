with Text_IO;
use Text_IO;

procedure Naptar is

   Nap : Positive;
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
      when others => Put_Line ( "Egy h�ten csak 7 nap van!" );
   end case;
end Naptar;