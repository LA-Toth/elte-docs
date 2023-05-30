generic
   type Szam is private;
   type Index is ( <> );
   type Vektor is array ( Index range <> ) of Szam;

   Zerus : in Szam;

   with function "+" ( A, B : Szam ) return Szam is <>;

function Szumma ( V : Vektor ) return Szam;