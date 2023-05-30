with Ada.Text_IO;
use Ada.Text_IO;

procedure Max_kereses is

    type Index is new Integer;
    type Elem is new Integer;
    type Tömb is array (Index range <>) of Elem;

    function Max_Hely ( T: Tömb ) return Index is
        Mh: Index := T'First;
    begin
        for I in T'Range loop
            if T(Mh) < T(I) then
                Mh := I;
            end if;
        end loop;
        return Mh;
    end Max_Hely;

    T0: Tömb(1..5);
    T1: Tömb(1..5) := (3,2,5,7,1);
    T2: Tömb := (3,2,5,7,1);
    -- T3: Tömb;

begin

    T0 := (3,2,5,7,1);
    Put_Line( Index'Image( Max_Hely(T0) ) );
    Put_Line( Index'Image( Max_Hely(T1) ) );
    Put_Line( Index'Image( Max_Hely(T2) ) );
    Put_Line( Index'Image( Max_Hely((3,2,5,7,1)) ) );

end Max_kereses;
