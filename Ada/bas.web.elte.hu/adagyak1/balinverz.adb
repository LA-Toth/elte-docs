procedure Balinverz ( A : in Felcsoport; B : out Felcsoport;
                      Van : out Boolean ) is
begin
   for X in Felcsoport'Range loop
      if X * A = Egyseg then
         Van := True;
         B := X;
         return;
      end if;
   end loop;
   Van := False;
end Balinverz;