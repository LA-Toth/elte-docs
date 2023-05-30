with Text_IO;
use Text_IO;

procedure Kocsma1 is 

   task type Reszeg;

   task Ajto is
      entry Belep; 
      entry Kilep; 
   end Ajto;

   task body Ajto is
      Benn : Natural := 0;  
   begin
      loop
         select
            accept Kilep; 
            Benn := Benn - 1;
         or when Benn < 5 =>
            accept Belep; 
            Benn := Benn + 1;
         or
            terminate;
         end select;
      end loop;
   end Ajto;

   task body Reszeg is
   begin
      loop
         select
            Ajto.Belep;
            Put_Line ( "Reszeg iszik" );
            delay 10.0;
            Ajto.Kilep;
            exit;
         else
            Put_Line ( "Reszeg: Hat akkor alszom egyet a parkban..." );
            delay 5.0;
         end select;
      end loop;
   end Reszeg;

   Reszegek : array ( 1 .. 7 ) of Reszeg;  

begin
   null;
end Kocsma1;