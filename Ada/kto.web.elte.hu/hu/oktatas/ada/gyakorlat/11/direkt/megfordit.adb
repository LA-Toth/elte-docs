with Ada.Direct_IO, Ada.Integer_Text_IO, Ada.Text_IO, Ada.Command_Line;
use Ada.Integer_Text_IO, Ada.Text_IO, Ada.Command_Line;

procedure Megfordit is

  package Int_Direct_IO is new Ada.Direct_IO(Integer);
  use Int_Direct_IO;
	
  procedure Megford�t ( F: in Int_Direct_IO.File_Type ) is
        N, M: Integer;
        I, J: Int_Direct_IO.Count;
  begin
        I := 1;
        J := Size(F);
        while I < J loop
            Read(F,N,I);
            Read(F,M,J);
            Write(F,M,I);
            Write(F,N,J);
            I := I+1;
            J := J-1;
        end loop;
  end Megford�t;
 
  procedure Ki�r ( F: in Int_Direct_IO.File_Type ) is
      N: Integer;
  begin
      while not End_Of_File(F) loop
          Read(F,N);
          Put(N);
          New_Line;
      end loop;
  end Ki�r;

  F: Int_Direct_IO.File_Type;

begin
   if Argument_Count = 1 then
      Open(F, Name => Argument(1), Mode => InOut_File);
      declare begin
          Megford�t(F);
      exception
          when others => Close(F); raise;
      end;
      Reset(F, Mode => In_File);
      declare begin
          Ki�r(F);
      exception
          when others => Close(F); raise;
      end;
      Close(F);
  else 
      Put_Line(Current_Error, "Argumentum: az adatf�jl neve.");
  end if;
end Megfordit;

