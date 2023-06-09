package Racionalisok is

    type Racionális is private;

    function Számláló ( R: Racionális ) return Integer;
    function Nevező ( R: Racionális ) return Positive;

    function "/" ( Számláló: Integer; Nevező: Positive ) return Racionális;
    function "/" ( X, Y: Racionális ) return Racionális;
    function "/" ( X: Racionális; Y: Positive ) return Racionális;

    -- function "=" ( X, Y: Racionális ) return Boolean;

private

    type Racionális is record
                           Számláló: Integer := 0;
                           Nevező: Positive := 1;
                       end record;

end Racionalisok;
