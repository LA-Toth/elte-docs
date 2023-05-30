function Szumma ( V : Vektor ) return Szam is 
   S : Szam := Zerus;
begin
   for I in V'Range loop
      S := S + V ( I );
   end loop;
   return S;
end Szumma;