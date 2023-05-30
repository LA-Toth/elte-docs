with Text_IO;
use Text_IO;

procedure Lin_Ker4 is
   function Talalt ( Szoveg : String; Ch : Character; Poz : Natural )
      return Boolean is
   begin
      return ( Szoveg ( Poz + 1 ) = Ch );
   end Talalt;
   
   S : constant String := "Hello Világ!";
   C : constant Character := 'o';
   P : Natural := S'First - 1;
   V, L : Boolean := False;
begin
   V := ( P + 1 >= S'Last );
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
end Lin_Ker4;                         