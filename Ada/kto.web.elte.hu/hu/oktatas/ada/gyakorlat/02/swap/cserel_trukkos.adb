procedure Cserel_Trukkos ( A, B : in out Integer ) is
begin
   A := A + B;
   B := A - B;
   A := A - B;
end Cserel_Trukkos;
