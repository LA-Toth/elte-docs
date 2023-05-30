generic
    type Elem is private;
    type Index is (<>);
    type Tömb is array ( Index range <> ) of Elem;
    --type Tömb is array ( Index ) of Elem;
    with function Op( A, B: Elem ) return Elem;
    Start: in Elem;

function Fold ( T: Tömb ) return Elem;

