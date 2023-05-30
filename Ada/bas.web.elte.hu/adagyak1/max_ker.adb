with Text_IO;
use Text_IO;

procedure Max_Ker is 
   type Int_Vektor is array ( 1 .. 10 ) of Integer;

   IV : constant Int_Vektor := ( 2, 6, 5, - 9, 1, 56, 86, 23, - 71, 4 );

   function Max_Hely ( V : Int_Vektor ) return Positive is 
      MH : Positive := 1;
   begin
      for I in 1 .. 10 loop
         if V ( MH ) < V ( I ) then
            MH := I;
         end if;
      end loop;
      return MH;
   end Max_Hely;
begin
   Put_Line ( "A maximum: " & Integer'Image ( IV ( Max_Hely ( IV ))));
end Max_Ker;                    