function Max3 ( A, B, C : Integer ) return Integer is

begin
   if A >= B and then A >= C then
      return A;
   elsif B >= A and then B >= C then
      return B;
   else
      return C;
   end if;
end Max3;