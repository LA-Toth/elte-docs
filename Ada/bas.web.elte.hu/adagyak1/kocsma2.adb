with Text_IO;
use Text_IO;

procedure Kocsma2 is 

   type Ital is ( Sor, Bor, Palinka );

   task type Reszeg;

   task Kocsmaros is
      entry Tolt ( Mit : in Ital );
   end Kocsmaros;

   task Ajto is
      entry Belep; 
      entry Kilep; 
   end Ajto;

   task body Kocsmaros is
      Toltido : constant array ( Ital ) of Duration :=
                ( Sor => 1.0,
                  Bor => 0.1,
                  Palinka => 0.2 );
      Zaras_Varas : constant Duration := 5.0;  
   begin
      loop
         select
            accept Tolt ( Mit : in Ital ) do 
               Put_Line( "Kocsmaros: " & Ital'Image ( Mit ) & "-t toltok" );
               delay Toltido ( Mit );
            end Tolt;
         or
            delay Zaras_Varas;
            exit;
         end select;
      end loop;
   end Kocsmaros;

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
      Ivasido : constant
      array ( Ital ) of Duration := ( Sor => 2.0,
                                      Bor => 1.0,
                                      Palinka => 0.2 );
      Idofaktor : Positive := 1;  
   begin
      loop
         select
            Ajto.Belep;
            Kocsmaros.Tolt ( Palinka );
            Put_Line ( "Reszeg palinkat iszik" );
            delay Ivasido ( Palinka );
            Kocsmaros.Tolt ( Bor );
            Put_Line ( "Reszeg bort iszik" );
            delay Ivasido ( Bor );
            loop
               Kocsmaros.Tolt ( Sor );
               Put_Line ( "Reszeg sort iszik" );
               delay Ivasido ( Sor ) * Idofaktor;
               Idofaktor := Idofaktor + 1;
            end loop;
         else
            Put_Line ( "Reszeg: Hat akkor alszom egyet a parkban..." );
            delay 5.0;
         end select;
      end loop;
   exception
      when Tasking_Error =>
         Put_Line( "Reszeg: Bezart?" );
         Ajto.Kilep;
   end Reszeg;

   Reszegek : array ( 1 .. 7 ) of Reszeg;  

begin
   null;
end Kocsma2;