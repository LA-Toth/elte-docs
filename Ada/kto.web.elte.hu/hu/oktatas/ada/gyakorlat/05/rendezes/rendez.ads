generic
    type Elem is private;
    type Index is (<>);
    type Tömb is array (Index range <>) of Elem;
    with function "<" ( A, B: Elem ) return Boolean is <>;

procedure Rendez ( T: in out Tömb );
