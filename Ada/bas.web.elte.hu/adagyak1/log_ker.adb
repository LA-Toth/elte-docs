with Text_IO;
use Text_IO;

procedure Log_Ker is
   S : constant String := "acfghmopqz";
   C : constant Character := 'o';
   P : Natural := S'First;
   Q : Natural := S'Last;
   K : Natural := ( P + Q ) / 2;
   V, L : Boolean := False;
begin
   while ( not V ) and then ( not L ) loop
      V := ( P > Q );
      if S ( K ) < C then
         P := K + 1;
      elsif S ( K ) > C then
         Q := K - 1;
      else
         L := True;
      end if;
      K := ( P + Q ) / 2;
   end loop;
   if L then
      Put_Line( "Megvan." );
   else
      Put_Line( "Nincs meg." );
   end if;
end Log_Ker;
                    