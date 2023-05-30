with Fold, Ada.Text_IO;
use Ada.Text_IO;

procedure Szummazas is

    type Index is new Integer;
    type Elem is new Integer;
    type T�mb is array (Index range <>) of Elem;

    function Sum is new Fold(Elem, Index, T�mb, "+", 0);
    function Prod is new Fold(Elem, Index, T�mb, "*", 1);

begin

    Put_Line( Elem'Image( Sum((3,2,5,7,1)) ) );
    Put_Line( Elem'Image( Prod((3,2,5,7,1)) ) );

end Szummazas;
