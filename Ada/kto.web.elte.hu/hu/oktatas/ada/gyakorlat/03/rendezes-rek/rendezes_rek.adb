with Ada.Text_IO;
use Ada.Text_IO;

procedure Rendezes_Rek is    
	
    type Index is new Integer;
    type Elem is new Integer;
    type T�mb is array (Index range <>) of Elem;

    function Max_Hely ( T: T�mb ) return Index is
        Mh: Index := T'First;
    begin
        for I in T'Range loop
            if T(Mh) < T(I) then
                Mh := I;
            end if;
        end loop;
        return Mh;
    end Max_Hely;

    procedure Cser�l ( A, B: in out Elem ) is
        Tmp: Elem := A;
    begin
        A := B;
        B := Tmp;
    end Cser�l;
    
    procedure Rendez ( T: in out T�mb ) is
        Mh: Index;
    begin
        if T'Length > 1 then
            Mh := Max_Hely( T );
	    Cser�l( T(T'Last), T(Mh) );
	    Rendez( T(T'First..Index'Pred(T'Last)) );
        end if;
    end Rendez;

    T: T�mb := (3,6,1,5,3);

begin
    Rendez(T);
    for I in T'Range loop
       Put_Line( Elem'Image( T(I) ) );
    end loop;
end Rendezes_Rek;
