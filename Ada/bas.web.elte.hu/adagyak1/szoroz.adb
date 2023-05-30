with Text_IO, Ada.Command_Line;
use Text_IO, Ada.Command_Line;

procedure Szoroz is

begin
   if Argument_Count < 2 then
      Put_Line ( "Adj meg két egész számot a parancssorban!" );
   else
      Put_Line ( Integer'Image ( Integer'Value ( Argument ( 1 )) *
                                 Integer'Value ( Argument ( 2 ))));
   end if;
end Szoroz;