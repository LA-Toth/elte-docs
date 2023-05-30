with Text_IO;
use Text_IO;

procedure Max_Ker2 is 
   type Index is new Integer range 1 .. 10;
   type Int_Vektor is array ( Index ) of Integer;

   IV : constant Int_Vektor := ( 3 => 0, 4 .. 6 => 9, 8 => 13,
                                     7 | 9 => 4, 1 | 2 | 10 => 2 );

   function Max_Hely ( V : Int_Vektor ) return Index is 
      MH : Index := 1;
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
end Max_Ker2;                      