with Ada.Text_IO, Ada.Calendar, Ada.Numerics.Discrete_Random, Taszkseged;
use Ada.Text_IO, Taszkseged;

procedure Kocsma is

  Ajto: Taszkseged.Szemafor(4);
  procedure Belep renames Ajto.P;
  procedure Kilep renames Ajto.V;
  
  type Ital is (Sor, Bor, Palinka);

  task Kocsmaros is
     entry Tolt( Mit: in Ital );
  end Kocsmaros;

  task body Kocsmaros is
     Toltes_Ido: constant array(Ital) of Duration := (1.0,0.5,0.2);
     Hazamegy: Boolean := False;
  begin
     while not Hazamegy loop
        select
           accept Tolt( Mit: in Ital ) do
	     Put_Line("Töltök " & Ital'Image(Mit) & "t.");
	     delay Toltes_Ido(Mit);
             --case Mit is
             --    when Sor     => delay 1.0;
             --    when Bor     => delay 0.5;
             --    when Palinka => delay 0.2;
	     --end case;
	  end Tolt;
	or
	  delay 3.0;
	  Hazamegy := True;
        end select;
     end loop;
     Put_Line("A kocsmáros hazamegy.");
  end Kocsmaros;

  type String_Access is access String;

  task type Reszeg is
     entry Start( Nev: in String_Access );
  end Reszeg;

  task body Reszeg is
     Sor_Ivas: Duration := 2.0;
     Nevem: String_Access;
     Bejutottam: Boolean := False;
  begin 
     accept Start( Nev: in String_Access ) do
        Nevem := Nev;
     end;
     Put_Line("Feltűnik egy részeg, " & Nevem.all & ".");
     while not Bejutottam loop
       select
         Ajto.P; --Belep;
	 Bejutottam := True;
       else
         Put_Line("A részeg, " & Nevem.all & ", elmegy a parkba aludni egyet.");
         delay 1.2;
       end select;
     end loop;
     Put_Line("Részeg lépett be a kocsmába, " & Nevem.all & ".");
     Kocsmaros.Tolt(Palinka);
     delay 0.1;
     Kocsmaros.Tolt(Bor);
     delay 0.3;
     loop
         Kocsmaros.Tolt(Sor);
         delay Sor_Ivas;
	 Sor_Ivas := 2*Sor_Ivas;
     end loop;
  exception
     when Tasking_Error => Put_Line("A részeg, " & Nevem.all & ", keres egy másik kocsmát."); Kilep;
  end Reszeg;
  
  Reszegek: array (1..10) of Reszeg;

  task type Egyetemista( Nev: String_Access; Mit_Iszik: Ital );
  type Egyetemista_Access is access Egyetemista;

  task body Egyetemista is
     F: Float;
  begin
     Put_Line("Benéz egy egyetemista, " & Nev.all & ".");
     select
       Ajto.P;
       Kocsmaros.Tolt(Mit_Iszik);
       Veletlen.General(F);
       delay 0.1 + Duration(F);
       Put_Line(Nev.all & " hazamegy.");
       Kilep;
     or
       delay 1.1;
       Put_Line(Nev.all & " elmegy analízis előadásra.");
     end select;
  end Egyetemista;
  
  E: Egyetemista_Access;

  Eloadas: Ada.Calendar.Time := Ada.Calendar."+"( Ada.Calendar.Clock, 1.0 );

  task Professzor;

  task body Professzor is
  begin
     Put_Line("Professzor: van egy kis időm az előadásom előtt.");
     select
       Ajto.P;
       Kocsmaros.Tolt(Palinka);
       delay 0.2;
       Put_Line("Professzor: így könnyebb elviselni a hallgatókat.");
       Kilep;
     or
       delay until Eloadas;
       Put_Line("Professzor: megyek, mert elkések az analízis előadásról.");
     end select;
  end Professzor;

  package Ital_Random is new Ada.Numerics.Discrete_Random(Ital);
  G: Ital_Random.Generator;

  F: Float;
  
begin
  Ital_Random.Reset(G);
  for I in Reszegek'Range loop
     E := new Egyetemista( new String'("E-" & Integer'Image(I)), Ital_Random.Random(G) );
     Reszegek(I).Start(  new String'("R-" & Integer'Image(I))  );
     Veletlen.General(F);
     delay Duration(0.9 + F/5.0);
  end loop;
end Kocsma;

