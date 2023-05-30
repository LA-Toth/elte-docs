package Poz_Rac is
   type Pozrac is private;

   function "/" ( Számláló, Nevezõ : Positive ) return Pozrac;
   function "/" ( P, Q : Pozrac ) return Pozrac;
   function "/" ( P : Pozrac; N : Positive ) return Pozrac;
   function Számláló ( P : Pozrac ) return Positive;
   function Nevezõ ( P : Pozrac ) return Positive;

private

   type Pozrac is 
      record 
         Számláló, Nevezõ : Positive;
      end record;
end Poz_Rac;
                