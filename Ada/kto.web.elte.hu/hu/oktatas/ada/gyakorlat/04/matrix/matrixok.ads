package Matrixok is
	
    type Index is new Integer;
    type Elem is new Integer;
    type M�trix is array (Index range <>, Index range <>) of Elem;

    procedure Hozz�ad( A: in out M�trix; B: in M�trix );
    function "+"( A, B: M�trix ) return M�trix;
    function "*" ( A, B: M�trix ) return M�trix;
    function "*" ( E: Elem; M: M�trix ) return M�trix;
    procedure Beszoroz ( E: in Elem; M: in out M�trix );

end Matrixok;

