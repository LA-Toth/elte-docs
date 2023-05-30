procedure Felt_Max_Ker ( T: in Tömb; Van: out Boolean; Max: out Elem ) is
    Mh: Index;
begin
	Van := False;
	for I in T'Range loop
		if Feltétel(T(I)) then
			if Van then
				if T(Mh) < T(I) then Mh := I; end if;
			else
				Van := True; Mh := I;
			end if;
		end if;
	end loop;
	Max := T(Mh);
end Felt_Max_Ker;

