with Text_IO, Ada.Command_Line;
use Text_IO, Ada.Command_Line;

procedure Jancsi_Julcsa5 is

   type String_Mut is access String;
   task type Kiiro ( Nev : String_Mut := new String' ( "" ));

   task Szemafor is
      entry P; 
      entry V; 
   end Szemafor;

   task body Kiiro is
      procedure Kiir ( S : in String ) is
      begin
         for I in S'range loop
            Put ( S ( I ));
         end loop;
         New_Line;
      end Kiir;
   begin
      for I in Positive'range loop
         Szemafor.P;
         Kiir ( Nev.all & ": " & Positive'Image ( I ));
         Szemafor.V;
         if Argument_Count > 0 then
            delay Duration'Value ( Argument ( 1 ));
         end if;
      end loop;
   end Kiiro;

   task body Szemafor is
   begin
      loop
         accept P;
         accept V;
      end loop;
   end Szemafor;

   Jancsi : Kiiro ( new String' ( "Jancsi" ));
   Julcsa : Kiiro ( new String' ( "Julcsa" ));

begin
   null;
end Jancsi_Julcsa5;