with Text_IO, Novel;
use Text_IO;

procedure Noveles is
   N : Integer := 6;
begin
   Put ( Integer'Image ( N ) & " + 1 = " );
   Novel ( N );
   Put_Line ( Integer'Image ( N ));
end Noveles;