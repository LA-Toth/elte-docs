with Matrixok, Text_IO;
use Matrixok, Text_IO;

procedure Matrixok_Proba is 
   M, N, K : Index;

   procedure Kiir ( M : in Matrix; Str : in String := "" ) is
   begin
      Put_Line ( Str );
      for I in M'Range ( 1 ) loop
         for J in M'Range ( 2 ) loop
            Put ( Integer'Image ( Integer ( M ( I, J ))));
            Put ( Ascii.HT );
         end loop;
         New_Line;
      end loop;
   end Kiir;

   procedure Beolvas ( M : out Matrix; Str : in String := "" ) is
      Sz : String ( 1 .. Integer'Width + 1 );
      Hossz : Natural;
   begin
      Put_Line ( Str );
      for I in M'Range ( 1 ) loop
         for J in M'Range ( 2 ) loop
            Put ( Index'Image ( I ) & ',' & Index'Image ( J ) & ": " );
            Get_Line ( Sz, Hossz );
            M ( I, J ) := Ertek ( Integer'Value ( Sz ( 1 .. Hossz )));
         end loop;
      end loop;
   end Beolvas;

   Sz : String ( 1 .. Integer'Width + 1 );
   Hossz : Natural;

begin
   New_Line ( 2 );
   Put_Line ( "Matrixszorzas..." );
   New_Line;
   Put_Line ( "Usd be a matrixok dimenziojat: " );
   Put ( "m = " );
   Get_Line ( Sz, Hossz );
   M := Index ( Integer'Value ( Sz ( 1 .. Hossz )));
   Put ( "n = " );
   Get_Line ( Sz, Hossz );
   N := Index ( Integer'Value ( Sz ( 1 .. Hossz )));
   Put ( "k = " );
   Get_Line ( Sz, Hossz );
   K := Index ( Integer'Value ( Sz ( 1 .. Hossz )));
   declare
      A : Matrix ( 1 .. M, 1 .. N );
      B : Matrix ( 1 .. N, 1 .. K );
   begin
      Beolvas ( A, "Az elso matrix:" );
      Beolvas ( B, "A masodik matrix:" );
      Kiir ( A * B, "Az eredmeny:" );
   end;
end Matrixok_Proba;
