with Seged, Ada.Calendar; use Seged, Ada.Calendar;

procedure Kocsma is

    Ajtó: Szemafor(5);
          -- Belépés: P
          -- Kilépés: V

    type Ital is (Sör, Bor, Pálinka);

    task Kocsmáros is
        entry Tölt( Mit: in Ital );
        entry Borravaló;
    end Kocsmáros;

    task body Kocsmáros is
        Munkaidõ_Vége: Time := Clock + 10.0;
        -- Lejárt_A_Munkaidõ: Boolean := False;
        Borravalók_Száma: Natural := 0;
    begin
        -- while not Lejárt_A_Munkaidõ loop
        while Clock < Munkaidõ_Vége loop       -- rövidebb túlóra
            select
               accept Tölt ( Mit: in Ital ) do
                  Put_Line("Töltök " & Ital'Image(Mit) & 't');
                  case Mit is
                       when Sör => delay 1.0;
                       when Bor => delay 0.2;
                       when Pálinka => delay 0.3;
                  end case;
               end Tölt;
               select
                  accept Borravaló; Borravalók_Száma := Borravalók_Száma + 1;
               or delay 0.2;
               end select;
            or delay until Munkaidõ_Vége;
               -- Lejárt_A_Munkaidõ := True;
            end select;
        end loop;
        Put_Line( "Na, én hazamegyek. összeszedtem " &
                  Natural'Image(Borravalók_Száma) & " borravalót.");
    end Kocsmáros;

    task type Részeg;
    task body Részeg is
        Sörivás_Ideje: Duration := 1.0;
    begin
        loop
           select
              Ajtó.P;  -- Belép
              Kocsmáros.Tölt(Pálinka);
              Put_Line("Benyomok egy felest.");
              delay 0.1;
              Kocsmáros.Tölt(Bor);
              Put_Line("Benyomok egy pohár bort.");
              delay 0.3;
              loop
                  Kocsmáros.Tölt(Sör);
                  Put_Line("Benyomok egy korsó sört.");
                  delay Sörivás_Ideje;
                  Sörivás_Ideje := 2 * Sörivás_Ideje;
              end loop;
           else
              Put_Line("Elmegyek a parkba szunyálni.");
              delay 1.0;
           end select;
        end loop;
    exception
        when Tasking_Error => Put_Line("Keresek egy másik helyet.");
                              Ajtó.V;  -- Kilép
    end Részeg;
    type Részeg_Access is access Részeg;
    R: Részeg_Access;

    type PString is access String;
    task type Egyetemista ( Név: PString );
    task body Egyetemista is
    begin
        select
            Ajtó.P;
            Kocsmáros.Tölt(Bor);
            select
                 Kocsmáros.Borravaló;
            or delay 0.05;
               Put_Line("Ha nem kell, hát nem kell.");
            end select;
            Put_Line(Név.all & " borozik.");
            delay 1.5;
            Ajtó.V;
         or delay 1.0;
            Put_Line("Inkább elmegyek Ada elõadásra.");
         end select;
    exception
        when Tasking_Error => Put_Line("Keresek egy másik helyet.");
                              Ajtó.V;  -- Kilép
    end Egyetemista;
    type Egyetemista_Access is access Egyetemista;
    E: Egyetemista_Access;
    Nevek: constant array (1..10) of PString := (
             new String'("Jani"), new String'("Peti"), new String'("Mari"), 
             new String'("Juci"), new String'("Béla"), new String'("Gabi"), 
             new String'("Zoli"), new String'("Dani"), new String'("Géza"),
             new String'("Rozi") );

begin

    for I in 1..10 loop
        delay 0.5;
        if Véletlen < 0.5 then
           Put_Line("Egy részeg tévedt erre.");
           R := new Részeg;
        else
           Put_Line("Egy egyetemista tévedt erre.");
           E := new Egyetemista(Nevek(Integer(Véletlen*10.0+0.5)));
        end if;
    end loop;

end Kocsma;

