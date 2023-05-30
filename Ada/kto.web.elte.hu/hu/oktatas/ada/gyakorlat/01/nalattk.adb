with Ada.Integer_Text_IO;
procedure Nalattk is
   N, K : Positive;
   N_Alatt_K: Positive := 1;
begin
   Ada.Integer_Text_IO.Get( N );
   Ada.Integer_Text_IO.Get( K );
   for I in 1..K loop
       N_Alatt_K := N_Alatt_K * (N-I+1) / I;
   end loop;
   Ada.Integer_Text_IO.Put( N_Alatt_K );
end Nalattk;
