with Text_IO, Ada.Command_Line;
use Text_IO, Ada.Command_Line;

procedure Jancsi_Julcsa6 is

   type String_Mut is access String;
   task type Kiiro ( Nev : String_Mut := new String' ( "" ));

   protected Vedett is
      procedure Kiir ( S : in String );
   end Vedett;

   task body Kiiro is
   begin
      for I in Positive'range loop
         Vedett.Kiir ( Nev.all & ": " & Positive'Image ( I ));
         if Argument_Count > 0 then
            delay Duration'Value ( Argument ( 1 ));
         end if;
      end loop;
   end Kiiro;

   protected body Vedett is
      procedure Kiir ( S : in String ) is
      begin
         for I in S'range loop
            Put ( S ( I ));
         end loop;
         New_Line;
      end Kiir;
   end Vedett;

   Jancsi : Kiiro ( new String' ( "Jancsi" ));
   Julcsa : Kiiro ( new String' ( "Julcsa" ));

begin
   null;
end Jancsi_Julcsa6;