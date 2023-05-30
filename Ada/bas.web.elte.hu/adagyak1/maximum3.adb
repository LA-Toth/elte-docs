with Text_IO, Max3;

use Text_IO;

procedure Maximum3 is

begin
   Put_Line ( "4, 6 és 11 maximuma: " & Integer'Image ( Max3 ( 4, 6, 11 )));
end Maximum3;