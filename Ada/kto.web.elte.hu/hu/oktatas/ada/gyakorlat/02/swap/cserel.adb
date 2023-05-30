procedure Cserel ( A, B : in out Integer ) is
   Tmp: Integer := A;
begin
   A := B;
   B := Tmp;
end Cserel;
