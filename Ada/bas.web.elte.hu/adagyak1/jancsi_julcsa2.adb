with Text_IO;
use Text_IO;
procedure Jancsi_Julcsa2 is 

   task type Kiiro is
      entry Init ( S : in String := "" );
   end Kiiro;

   task body Kiiro is
      type Pstr is access String; 
      Nev : Pstr;  
   begin
      accept Init ( S : in String := "" ) do
         Nev := new String ( 1 .. S'Length );
         Nev.all := S;
      end Init;
      for I in Positive'range loop
         Put_Line ( Nev.all & ": " & Positive'Image ( I ));
      end loop;
   end Kiiro;

   Jancsi, Julcsa : Kiiro;

begin
   Jancsi.Init( "Jancsi" );
   Julcsa.Init( "Julcsa" );
end Jancsi_Julcsa2;                          