package Matrixok is
	
    type Index is new Integer;
    type Elem is new Integer;
    type Mátrix is array (Index range <>, Index range <>) of Elem;

    procedure Hozzáad( A: in out Mátrix; B: in Mátrix );
    function "+"( A, B: Mátrix ) return Mátrix;
    function "*" ( A, B: Mátrix ) return Mátrix;
    function "*" ( E: Elem; M: Mátrix ) return Mátrix;
    procedure Beszoroz ( E: in Elem; M: in out Mátrix );

end Matrixok;

