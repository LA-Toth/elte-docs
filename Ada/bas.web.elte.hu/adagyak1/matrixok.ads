package Matrixok is
   type Index is new Integer;
   type Ertek is new Float digits 5;
   type Matrix is array ( Index range <>, Index range <> ) of Ertek;

   function "+" ( A, B : Matrix ) return Matrix;
   function "*" ( A, B : Matrix ) return Matrix;
end Matrixok;
