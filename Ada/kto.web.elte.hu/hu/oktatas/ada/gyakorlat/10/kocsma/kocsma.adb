with Ada.Text_IO; use Ada.Text_IO;

procedure Kocsma is

    type Ital is (Sör, Bor, Pálinka);

    task Kocsmáros is
        entry Tölt( Mit: in Ital );
    end Kocsmáros;

    task body Kocsmáros is
    begin
        for I in 1..20 loop
            accept Tölt ( Mit: in Ital ) do
                Put_Line("Töltök " & Ital'Image(Mit) & 't');
                case Mit is
                    when Sör => delay 1.0;
                    when Bor => delay 0.2;
                    when Pálinka => delay 0.3;
                end case;
            end Tölt;
        end loop;
    end Kocsmáros;

    task type Részeg;

    task body Részeg is
        Sörivás_Ideje: Duration := 1.0;
    begin
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
    end Részeg;

    type Részeg_Access is access Részeg;
    R: Részeg_Access;

begin

    for I in 1..5 loop
        delay 3.0;
        Put_Line("Egy részeg tévedt erre.");
        R := new Részeg;
    end loop;

end Kocsma;

