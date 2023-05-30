with Vermek, Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure Menu is
    package Int_Vermek is new Vermek(Integer); use Int_Vermek;
    Max: Integer;

    procedure Menu_Ciklus ( V: in out Verem ) is
        Parancs: Integer;

        procedure Betesz( V: in out Verem ) is
             Adat: Integer;
        begin
             Put("Mit akar betenni? "); Get(Adat);
             Push(V,Adat);
        exception
             when Tele_A_Verem =>
                     Put_Line("Nem tudom végrehajtani. Már megtelt a verem.");
        end Betesz;

        procedure Kivesz( V: in out Verem ) is
             Adat: Integer;
        begin
             Pop(V,Adat);
             Put("A kivett adat: "); Put(Adat); New_Line;
        exception
             when Üres_A_Verem =>
                     Put_Line("Nem tudom végrehajtani. Üres a verem.");
        end Kivesz;

    begin
        loop
            Put_Line("Elvégezhetõ tevékenységek: push (1), pop (2), quit (0)");
            Get(Parancs);
            case Parancs is
                when 1 => Betesz(V);
                when 2 => Kivesz(V);
                when 0 => exit;
                when others => null;
            end case;
        end loop;
    end Menu_Ciklus;

begin
    Put( "Mekkora vermet szeretne használni? " ); Get(Max);
    declare
        V: Verem(Max);
    begin
        Menu_Ciklus(V);
    end;
end Menu;

