with Max_Hely, Cserel;
procedure Rendez ( T: in out T�mb ) is
    procedure Cser�l_Elem is new Cserel(Elem);
    function Max_Hely_T�mb is new Max_Hely(Elem,Index,T�mb);
    Mh: Index;
begin
    for I in reverse T'Range loop
        Mh := Max_Hely_T�mb( T(T'First..I) );
        Cser�l_Elem( T(I), T(Mh) );
    end loop;
end Rendez;
