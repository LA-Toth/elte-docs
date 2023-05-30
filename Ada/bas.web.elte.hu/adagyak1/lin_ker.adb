with Text_IO;
use Text_IO;

procedure Lin_Ker is
   S : constant String := "Hello Világ!";
   C : constant Character := 'o';
   P : Natural := S'First - 1;
   V, L : Boolean := False;
begin
   while ( not V ) and then ( not L ) loop
      V := ( P + 1 >= S'Last );
      L := ( S ( P + 1 ) = C );
      P := P + 1;
   end loop;
   if L then
      Put_Line( "Megvan." );
   else
      Put_Line( "Nincs meg." );
   end if;
end Lin_Ker;
