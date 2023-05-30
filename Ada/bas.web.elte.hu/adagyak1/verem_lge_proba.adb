with Verem_LGE, Text_IO;
use Text_IO;

procedure Verem_LGE_Proba is
   package Int_Verem is new Verem_LGE ( Integer );

   use Int_Verem;

   V : Verem;
begin
   begin
      Put_Line ( "A verem teteje: " &
                 Integer'Image ( Tet� ( V )));
   exception
      when �res_Verem =>
         Put_Line ( "A verem �res." );
   end;
   for I in 1 .. 3 loop
      begin
         Betev�s ( V, I );
         Put_Line ( "Betett�k " & Integer'Image ( I ) & "-t." );
      end;
   end loop;
   Kidob�s ( V );
   Put_Line ( "A verem teteje: " &
              Integer'Image ( Tet� ( V )));
exception
   when others =>
      Put_Line ( "Valahol hiba t�rt�nt, ahol nem is v�rtuk." );
end Verem_LGE_Proba;