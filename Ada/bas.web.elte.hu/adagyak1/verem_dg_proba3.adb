with Verem_DG, Text_IO;
use Text_IO;

procedure Verem_DG_Proba3 is
   package Int_Verem is new Verem_DG ( Integer );

   use Int_Verem;


   Mélység : String ( 1 .. 10 );
   Mélység_Hossz : Natural;
begin
   Put ( "A verem mérete: " );
   Get_Line ( Mélység, Mélység_Hossz );
   declare
      V : Verem ( Méret'Value ( Mélység ( 1 .. Mélység_Hossz )));
   begin
      begin
         Put_Line ( "A verem teteje: " &
                    Integer'Image ( Tetõ ( V )));
      exception
         when Constraint_Error =>
            Put_Line ( "A verem üres." );
      end;
      for I in 1 .. 3 loop
         begin
            Betevés ( V, I );
            Put_Line ( "Betettük " & Integer'Image ( I ) & "-t." );
         exception
            when Constraint_Error =>
               Put_Line ( "A verem megtelt." );
         end;
      end loop;
      Kidobás ( V );
      Put_Line ( "A verem teteje: " &
                 Integer'Image ( Tetõ ( V )));
   exception
      when others =>
         Put_Line ( "Valahol hiba történt, ahol nem is vártuk." );
   end;
end Verem_DG_Proba3;