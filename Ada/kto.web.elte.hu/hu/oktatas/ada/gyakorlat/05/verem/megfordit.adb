with Vermek, Integer_Vermek, Ada.Command_Line, Ada.Integer_Text_IO;
use Integer_Vermek;
procedure Megfordit is
    package Int_Vermek is new Vermek(Integer); use Int_Vermek;
    N: Integer;
    V: Int_Vermek.Verem(Ada.Command_Line.Argument_Count);
    W: Integer_Vermek.Verem(Ada.Command_Line.Argument_Count);
begin
    for I in 1..Ada.Command_Line.Argument_Count loop
       N := Integer'Value(Ada.Command_Line.Argument(I));
       Push( V, N );
    end loop;
    while not Is_Empty(V) loop 
        Pop( V, N );
        Push( W, N );
    end loop;
    while not Is_Empty(W) loop 
        Pop( W, N );
        Ada.Integer_Text_IO.Put(N);
    end loop;
end Megfordit;

