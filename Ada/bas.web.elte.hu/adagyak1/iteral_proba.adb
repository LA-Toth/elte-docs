with Iteral, Text_IO;
use Text_IO;

procedure Iteral_Proba is
   N : Integer := 5;
   L_Szam : Natural;

   function Vege ( N : Integer ) return Boolean is
   begin
      return N < 0;
   end Vege;

   procedure Atalakit ( N : in out Integer ) is
   begin
      Put_Line ( Integer'Image ( N ));
      N := N - 1;
   end Atalakit;

   procedure It is new Iteral ( Integer, Vege, Atalakit );
begin
   It ( N, L_Szam );
   Put_Line ( Natural'Image ( L_Szam ) & " iteraciot vegeztem." );
end Iteral_Proba;