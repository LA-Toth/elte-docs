with Text_IO, Ada.Command_Line;
use Text_IO, Ada.Command_Line;

procedure Jancsi_Julcsa3 is

   type String_Mut is access String;
   task type Kiiro ( Nev : String_Mut := new String' ( "" ));

   task body Kiiro is
   begin
      for I in Positive'range loop
         Put_Line ( Nev.all & ": " & Positive'Image ( I ));
         if Argument_Count > 0 then
            delay Duration'Value ( Argument ( 1 ));
         end if;
      end loop;
   end Kiiro;

   Jancsi : Kiiro ( new String' ( "Jancsi" ));
   Julcsa : Kiiro ( new String' ( "Julcsa" ));

begin
   null;
end Jancsi_Julcsa3;