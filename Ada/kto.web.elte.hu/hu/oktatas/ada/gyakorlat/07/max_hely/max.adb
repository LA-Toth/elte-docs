function Max ( T: T�mb ) return Elem is
begin
		if T ' Length = 0 then
			Raise_Exception( �res_T�mb ); --, "�res a t�mb" );
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

