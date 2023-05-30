with Text_IO, Sequential_IO;
use Text_IO;

procedure Olvas2 is

   package Int_Text_IO is new Text_IO.Integer_IO ( Integer );
   use Int_Text_IO;

   package Int_Bin_IO is new Sequential_IO ( Integer );
   use Int_Bin_IO;

   F : Int_Bin_IO.File_Type;
   I : Integer;
   J : Natural := 0;

begin
   Open( F, Name => "szamok.dat", Mode => In_File );
   while not End_Of_File ( F ) loop
      Read ( F, I );
      Put ( I );
      J := J + 1;
      if J = 5 then
         New_Line;
         J := 0;
      end if;
   end loop;
   Close ( F );
end Olvas2;