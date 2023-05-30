procedure Felt_Max_Ker ( T: in T�mb; Van: out Boolean; Max: out Elem ) is
    Mh: Index;
begin
	Van := False;
	for I in T'Range loop
		if Felt�tel(T(I)) then
			if Van then
				if T(Mh) < T(I) then Mh := I; end if;
			else
				Van := True; Mh := I;
			end if;
		end if;
	end loop;
	Max := T(Mh);
end Felt_Max_Ker;

