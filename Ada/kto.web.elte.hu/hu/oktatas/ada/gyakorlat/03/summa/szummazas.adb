with Ada.Text_IO;
use Ada.Text_IO;

procedure Szummazas is

    type Index is new Integer;
    type Elem is new Integer;
    type T�mb is array (Index range <>) of Elem;

    function Szumma ( T: T�mb ) return Elem is
        S: Elem := 0;
    begin
        for I in T'Range loop
	    S := S + T(I);
        end loop;
        return S;
    end Szumma;

begin

    Put_Line( Elem'Image( Szumma((3,2,5,7,1)) ) );

end Szummazas;
