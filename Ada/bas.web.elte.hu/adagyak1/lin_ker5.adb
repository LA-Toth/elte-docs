with Text_IO;
use Text_IO;

procedure Lin_Ker5 is
   S : constant String := "Hello Világ!";
   C : constant Character := 'o';
   P : Natural := S'First - 1;
   V : Boolean := ( P + 1 >= S'Last );
   L : Boolean := False;
   
   function Talalt return Boolean is
   begin
      return ( S ( P + 1 ) = C );
   end Talalt;
   
begin
   while ( not V ) and then ( not L ) loop
      L := Talalt;
      P := P + 1;
   end loop;
   if L then
      Put_Line( "Megvan." );
   else
      Put_Line( "Nincs meg." );
   end if;
end Lin_Ker5;                         