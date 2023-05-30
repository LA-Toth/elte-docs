function Max ( T: Tömb ) return Elem is
begin
		if T ' Length = 0 then
			Raise_Exception( Üres_Tömb ); --, "Üres a tömb" );
		else declare
			Mh: Index := T ' First;
		        begin
				for I in T'Range loop
					if T(Mh) < T(I) then Mh := I; end if;
				end loop;
				return T(Mh);
		        end; 		
		end if;
end Max;

