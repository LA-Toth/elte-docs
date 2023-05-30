with Verem_DG, Text_IO;
use Text_IO;

procedure Verem_DG_Proba3 is
   package Int_Verem is new Verem_DG ( Integer );

   use Int_Verem;


   M�lys�g : String ( 1 .. 10 );
   M�lys�g_Hossz : Natural;
begin
   Put ( "A verem m�rete: " );
   Get_Line ( M�lys�g, M�lys�g_Hossz );
   declare
      V : Verem ( M�ret'Value ( M�lys�g ( 1 .. M�lys�g_Hossz )));
   begin
      begin
         Put_Line ( "A verem teteje: " &
                    Integer'Image ( Tet� ( V )));
      exception
         when Constraint_Error =>
            Put_Line ( "A verem �res." );
      end;
      for I in 1 .. 3 loop
         begin
            Betev�s ( V, I );
            Put_Line ( "Betett�k " & Integer'Image ( I ) & "-t." );
         exception
            when Constraint_Error =>
               Put_Line ( "A verem megtelt." );
         end;
      end loop;
      Kidob�s ( V );
      Put_Line ( "A verem teteje: " &
                 Integer'Image ( Tet� ( V )));
   exception
      when others =>
         Put_Line ( "Valahol hiba t�rt�nt, ahol nem is v�rtuk." );
   end;
end Verem_DG_Proba3;