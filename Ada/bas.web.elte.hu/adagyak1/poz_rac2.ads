package Poz_Rac2 is
   type Pozrac is private;

   function "+" ( P : Pozrac; Q : Pozrac ) return Pozrac;
   function "-" ( P : Pozrac; Q : Pozrac ) return Pozrac;
   function "*" ( P, Q : Pozrac ) return Pozrac;
   function "*" ( P : Pozrac; N : Positive ) return Pozrac;
   function "*" ( N : Positive; P : Pozrac ) return Pozrac;
   function "/" ( Sz�ml�l�, Nevez� : Positive ) return Pozrac;
   function "/" ( P, Q : Pozrac ) return Pozrac;
   function "/" ( P : Pozrac; N : Positive ) return Pozrac;
   function "/" ( N : Positive; P : Pozrac ) return Pozrac;
   function Sz�ml�l� ( P : Pozrac ) return Positive;
   function Nevez� ( P : Pozrac ) return Positive;

private

   type Pozrac is 
      record 
         Sz�ml�l�, Nevez� : Positive;
      end record;
end Poz_Rac2;                