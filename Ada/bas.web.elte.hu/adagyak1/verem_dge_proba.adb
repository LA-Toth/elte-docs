with Verem_DGE, Text_IO;
use Text_IO;

procedure Verem_DGE_Proba is
   package Int_Verem is new Verem_DGE ( Integer );

   use Int_Verem;

   V : Verem ( 2 );
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
     -- exception
      --   when Tele_Verem =>
       --     Put_Line ( "A verem megtelt." );
      end;
   end loop;
   Kidob�s ( V );
   Put_Line ( "A verem teteje: " &
              Integer'Image ( Tet� ( V )));
end Verem_DGE_Proba;