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
