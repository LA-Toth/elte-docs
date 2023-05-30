procedure Iteral ( E : in out Elem; Lepes_Szam : out Natural ) is
begin
   while not Vege ( E ) loop
      Atalakit ( E );
      Lepes_Szam := Lepes_Szam + 1;
   end loop;
end Iteral;