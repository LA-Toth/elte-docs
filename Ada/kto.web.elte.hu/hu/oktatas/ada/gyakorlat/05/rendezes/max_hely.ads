generic
    type Elem is limited private;
    type Index is (<>);
    type T�mb is array (Index range <>) of Elem;
    with function "<" ( A, B: Elem ) return Boolean is <>;

function Max_Hely ( T: T�mb ) return Index;
