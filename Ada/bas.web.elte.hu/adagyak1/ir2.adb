with Text_IO, Sequential_IO;
use Text_IO;

procedure Ir2 is

   package Int_Text_IO is new Text_IO.Integer_IO ( Integer );
   use Int_Text_IO;

   package Int_Bin_IO is new Sequential_IO ( Integer );
   use Int_Bin_IO;

   F : Int_Bin_IO.File_Type;
   I : Integer;

begin
   Create ( F, Name => "szamok.dat" );
   while not End_Of_File loop
      while not End_Of_Line loop
         Get ( I );
         Write ( F, I );
      end loop;
      Skip_Line;
   end loop;
   Close ( F );
end Ir2;