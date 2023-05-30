with Text_IO;

use Text_IO;

procedure Maximum3 is

   function Max ( A, B : Integer ) return Integer is

   begin
      if A >= B then
         return A;
      else
         return B;
      end if;
   end Max;

   function Max ( A, B, C : Integer ) return Integer is

   begin
      if A >= B and then A >= C then
         return A;
      elsif B >= A and then B >= C then
         return B;
      else
         return C;
      end if;
   end Max;

begin
   Put_Line ( "4 és 6 maximuma: " & Integer'Image ( Max ( 4, 6 )));
   Put_Line ( "4, 6 és 11 maximuma: " & Integer'Image ( Max ( 4, 6, 11 )));
end Maximum3;