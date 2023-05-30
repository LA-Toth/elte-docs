with Text_IO, Talalt;
use Text_IO;

procedure Lin_Ker3 is
   S : constant String := "Hello Világ!";
   C : constant Character := 'o';
   P : Natural := S'First - 1;
   V : Boolean := ( P + 1 >= S'Last );
   L : Boolean := False;
begin
   while ( not V ) and then ( not L ) loop
      V := ( P + 1 >= S'Last );
      L := Talalt ( S, C, P );
      P := P + 1;
   end loop;
   if L then
      Put_Line( "Megvan." );
   else
      Put_Line( "Nincs meg." );
   end if;
end Lin_Ker3;                   