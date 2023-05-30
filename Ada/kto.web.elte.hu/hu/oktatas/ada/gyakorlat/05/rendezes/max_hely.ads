generic
    type Elem is limited private;
    type Index is (<>);
    type Tömb is array (Index range <>) of Elem;
    with function "<" ( A, B: Elem ) return Boolean is <>;

function Max_Hely ( T: Tömb ) return Index;
