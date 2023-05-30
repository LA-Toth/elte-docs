with Verem_DGE, Text_IO;
use Text_IO;

procedure Verem_DGE_Proba is
   package Int_Verem is new Verem_DGE ( Integer );

   use Int_Verem;

   V : Verem ( 2 );
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
     -- exception
      --   when Tele_Verem =>
       --     Put_Line ( "A verem megtelt." );
      end;
   end loop;
   Kidobás ( V );
   Put_Line ( "A verem teteje: " &
              Integer'Image ( Tetõ ( V )));
end Verem_DGE_Proba;