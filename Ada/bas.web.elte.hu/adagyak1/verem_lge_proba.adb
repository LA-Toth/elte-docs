with Verem_LGE, Text_IO;
use Text_IO;

procedure Verem_LGE_Proba is
   package Int_Verem is new Verem_LGE ( Integer );

   use Int_Verem;

   V : Verem;
begin
   begin
      Put_Line ( "A verem teteje: " &
                 Integer'Image ( Tetõ ( V )));
   exception
      when Üres_Verem =>
         Put_Line ( "A verem üres." );
   end;
   for I in 1 .. 3 loop
      begin
         Betevés ( V, I );
         Put_Line ( "Betettük " & Integer'Image ( I ) & "-t." );
      end;
   end loop;
   Kidobás ( V );
   Put_Line ( "A verem teteje: " &
              Integer'Image ( Tetõ ( V )));
exception
   when others =>
      Put_Line ( "Valahol hiba történt, ahol nem is vártuk." );
end Verem_LGE_Proba;