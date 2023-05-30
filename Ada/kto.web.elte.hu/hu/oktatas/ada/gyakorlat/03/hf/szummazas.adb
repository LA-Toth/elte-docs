with Ada.Text_IO;
use Ada.Text_IO;

procedure Szummazas is

    type Index is new Integer;
    type Elem is new Integer;
    type Tömb is array (Index range <>) of Elem;

    function Szumma ( T: Tömb ) return Elem is
        S: Elem := 0;
    begin
        if T'Length = 0 then
            return 0;
        else
            return T(T'First) + Szumma(T(Index'Succ(T'First)..T'Last));
        end if;
    end Szumma;

begin

    Put_Line( Elem'Image( Szumma((3,2,5,7,1)) ) );

end Szummazas;
