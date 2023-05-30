with Text_IO, Novel2;
use Text_IO;

procedure Noveles2 is
   N : Integer := 6;
begin
   Put ( Integer'Image ( N ) & " + 1 = " );
   Novel2 ( N );
   Put_Line ( Integer'Image ( N ));
   Put ( Integer'Image ( N ) & " + 4 = " );
   Novel2 ( N, 4 );
   Put_Line ( Integer'Image ( N ));
   Put ( Integer'Image ( N ) & " + 6 = " );
   Novel2 ( Number => N, Diff => 6 );
   Put_Line ( Integer'Image ( N ));
   Put ( Integer'Image ( N ) & " + 8 = " );
   Novel2 ( Diff => 8, Number => N );
   Put_Line ( Integer'Image ( N ));
   Put ( Integer'Image ( N ) & " + 5 = " );
   Novel2 ( N, Diff => 5 );
   Put_Line ( Integer'Image ( N ));
   Put ( Integer'Image ( N ) & " + 1 = " );
   Novel2 ( Number => N );
   Put_Line ( Integer'Image ( N ));
end Noveles2;