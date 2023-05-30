with Max_Hely, Cserel;
procedure Rendez ( T: in out Tömb ) is
    procedure Cserél_Elem is new Cserel(Elem);
    function Max_Hely_Tömb is new Max_Hely(Elem,Index,Tömb);
    Mh: Index;
begin
    for I in reverse T'Range loop
        Mh := Max_Hely_Tömb( T(T'First..I) );
        Cserél_Elem( T(I), T(Mh) );
    end loop;
end Rendez;
