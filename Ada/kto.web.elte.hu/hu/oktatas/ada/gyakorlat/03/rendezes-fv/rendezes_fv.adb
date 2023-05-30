with Ada.Text_IO;
use Ada.Text_IO;

procedure Rendezes_Fv is
	
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

    procedure Cserél ( A, B: in out Elem ) is
        Tmp: Elem := A;
    begin
        A := B;
        B := Tmp;
    end Cserél;

    procedure Rendez ( T: in out Tömb ) is
        Mh: Index;
    begin
        for I in reverse T'Range loop
            Mh := Max_Hely( T(T'First..I) );
	    Cserél( T(I), T(Mh) );
        end loop;
    end Rendez;
    
    function Rendezve ( T: Tömb ) return Tömb is
        Új: Tömb := T;
    begin
        Rendez(Új);
        return Új;
    end Rendezve;

    T: Tömb := (3,6,1,5,3);

begin
    T := Rendezve(T);
    for I in T'Range loop
       Put_Line( Elem'Image( T(I) ) );
    end loop;
end Rendezes_Fv;
