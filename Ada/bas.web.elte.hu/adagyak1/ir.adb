with Text_IO;
use Text_IO;

procedure Ir is

   package Int_IO is new Integer_IO ( Integer );
   use Int_IO;

   F : File_Type;
   I : Integer;

begin
   Create ( F, Name => "szamok.txt" );
   while not End_Of_File loop
      while not End_Of_Line loop
         Get ( I );
         Put ( F, I );
      end loop;
      Skip_Line;
      New_Line ( F );
   end loop;
   Close ( F );
end Ir;