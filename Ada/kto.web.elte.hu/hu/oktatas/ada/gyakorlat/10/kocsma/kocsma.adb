with Ada.Text_IO; use Ada.Text_IO;

procedure Kocsma is

    type Ital is (S�r, Bor, P�linka);

    task Kocsm�ros is
        entry T�lt( Mit: in Ital );
    end Kocsm�ros;

    task body Kocsm�ros is
    begin
        for I in 1..20 loop
            accept T�lt ( Mit: in Ital ) do
                Put_Line("T�lt�k " & Ital'Image(Mit) & 't');
                case Mit is
                    when S�r => delay 1.0;
                    when Bor => delay 0.2;
                    when P�linka => delay 0.3;
                end case;
            end T�lt;
        end loop;
    end Kocsm�ros;

    task type R�szeg;

    task body R�szeg is
        S�riv�s_Ideje: Duration := 1.0;
    begin
        Kocsm�ros.T�lt(P�linka);
        Put_Line("Benyomok egy felest.");
        delay 0.1;
        Kocsm�ros.T�lt(Bor);
        Put_Line("Benyomok egy poh�r bort.");
        delay 0.3;
        loop
            Kocsm�ros.T�lt(S�r);
            Put_Line("Benyomok egy kors� s�rt.");
            delay S�riv�s_Ideje;
            S�riv�s_Ideje := 2 * S�riv�s_Ideje;
        end loop;
    end R�szeg;

    type R�szeg_Access is access R�szeg;
    R: R�szeg_Access;

begin

    for I in 1..5 loop
        delay 3.0;
        Put_Line("Egy r�szeg t�vedt erre.");
        R := new R�szeg;
    end loop;

end Kocsma;

