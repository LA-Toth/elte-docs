with Text_IO;
use Text_IO;

procedure Naptar4 is

   type Nap_T�pus is ( H�tf�, Kedd, Szerda, Cs�t�rt�k,
                       P�ntek, Szombat, Vas�rnap );

   Nap : Nap_T�pus;
   Sz�m : String ( 1 .. 9 );
   Sz�m_Hossza : Natural;

begin
   Put ( "Nap: " );
   Get_Line ( Sz�m, Sz�m_Hossza );
   Nap := Nap_T�pus'Value ( Sz�m ( 1 .. Sz�m_Hossza ));
   case Nap is
      when H�tf� => Put_Line ( "Els� munkanap." );
      when Szerda => Put_Line ( "K�z�ps� munkanap." );
      when P�ntek => Put_Line ( "Utols� munkanap." );
      when Kedd | Cs�t�rt�k => Put_Line ( "Sz�rke h�tk�znap." );
      when Szombat .. Vas�rnap => Put_Line ( "H�tv�ge." );
   end case;
end Naptar4;