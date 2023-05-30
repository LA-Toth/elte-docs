with Text_IO;
use Text_IO;

procedure Jancsi_Julcsa is 

   task Jancsi;
   task Julcsa;

   task body Jancsi is
   begin
      for I in 1 .. Positive'Last loop
         Put_Line ( "Jancsi:" & Positive'Image ( I ));
      end loop;
   end Jancsi;

   task body Julcsa is
   begin
      for I in 1 .. Positive'Last loop
         Put_Line ( "Julcsa:" & Positive'Image ( I ));
      end loop;
   end Julcsa;

begin
   null;
end Jancsi_Julcsa;