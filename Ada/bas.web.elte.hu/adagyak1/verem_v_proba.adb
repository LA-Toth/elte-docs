with Verem_V, Text_IO;
use Verem_V, Text_IO;

procedure Verem_V_Proba is 
   V : Verem;
begin
   Betevés ( V, 1 );
   Betevés ( V, 2 );
   Kidobás ( V );
   Put_Line ( Integer'Image ( Tetõ ( V )));
end Verem_V_Proba;          