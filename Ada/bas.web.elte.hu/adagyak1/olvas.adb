with Text_IO;
use Text_IO;

procedure Olvas is

   package Int_IO is new Integer_Io ( Integer );
   use Int_IO;

   F : File_Type;
   I : Integer;

begin
   Open( F, Name => "szamok.txt", Mode => In_File );
   while not End_Of_File ( F ) loop
      while not End_Of_Line ( F ) loop
         Get ( F, I );
         Put ( I );
      end loop;
      Skip_Line ( F );
      New_Line;
   end loop;
   Close ( F );
end Olvas;