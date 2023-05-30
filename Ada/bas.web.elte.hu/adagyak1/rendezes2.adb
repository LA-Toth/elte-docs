with Text_IO;
use Text_IO;

procedure Rendezes2 is
   type Index is new Positive;
   type Vektor is array ( Index range <> ) of Integer;

   V : Vektor := ( 100, 52, 99, 2, - 4 );

   function Max_Hely ( V : Vektor ) return Index is
      M : Index := V'First;
   begin
      for I in V'Range loop
         if V( M ) < V( I ) then
            M := I;
         end if;
      end loop;
      return M;
   end Max_Hely;

   procedure Cserel ( A, B : in out Integer ) is
      C : Integer;
   begin
      C := A;
      A := B;
      B := C;
   end Cserel;

   procedure Rendez ( V : in out Vektor ) is
      M : Index;
   begin
      if V'Length > 1 then
         M := Max_Hely ( V );
         Cserel ( V ( V'Last ), V ( M ));
         Rendez ( V ( V'First .. Index'Pred ( V'Last )));
      end if;
   end Rendez;
begin
   Rendez ( V );
   Put ( "A rendezett vektor: " );
   for I in V'Range loop
      Put ( Integer'Image ( V ( I )));
   end loop;
   New_Line;
end Rendezes2;