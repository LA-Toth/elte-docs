with Text_IO;
use Text_IO;

procedure Kocsma4 is 

   type String_P is access all String;
   type Ital is ( Sor, Bor, Palinka );

   task type Reszeg;
   task type Egyetemista ( Nev : String_P := null );

   type Egyetemista_P is access Egyetemista; 

   task Kocsmaros is
      entry Tolt ( Mit : in Ital );
   end Kocsmaros;

   protected Ajto is
      entry Belep; 
      procedure Kilep; 
      function Telitettseg return Float;
   private
      Max : Natural := 5;
      Bent : Natural := 0;
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

   protected body Ajto is
      entry Belep when Bent < Max is
      begin
         Bent := Bent + 1;
      end Belep;

      procedure Kilep is
      begin
         Bent := Bent - 1;
      end Kilep;

      function Telitettseg return Float is
      begin
         return Float ( Bent ) / Float ( Max );
      end Telitettseg;
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
         Put_Line( "Bezart?" );
         Ajto.Kilep;
   end Reszeg;

   task body Egyetemista is
   begin
      select
         Ajto.Belep;
         Kocsmaros.Tolt ( Bor );
         if Nev /= null then
            Put_Line ( Nev.all & " bort iszik" );
         else
            Put_Line ( "Egyetemista bort iszik" );
         end if;
         delay 3.0;
         Ajto.Kilep;
      or
         delay 1.0;
         if Nev /= null then
            Put_Line ( Nev.all & ": Akkor inkabb anal eloadasra megyek..." );
         else
            Put_Line ( "Egyetemista: Akkor inkabb anal eloadasra megyek..." );
         end if;
      end select;
   end Egyetemista;

   Egyetemistak : array ( 1 .. 3 ) of Egyetemista;  
   Reszegek : array ( 1 .. 7 ) of Reszeg;  
   Egy_Egyetemista : Egyetemista_P;  

begin
   delay 2.0;
   Egy_Egyetemista := new Egyetemista ( new String' ( "Jancsi" ));
   delay 1.0;
   Egy_Egyetemista := new Egyetemista ( new String' ( "Juliska" ));
end Kocsma4;