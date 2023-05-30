with Get_Discreet, Text_IO; use Text_IO;
procedure Proba is
   function Get_Int is new Get_Discreet(Integer);
begin
   Put_Line( Integer'Image(Get_Int) );
end Proba;
