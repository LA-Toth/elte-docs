package Racionalisok is

    type Racionális is private;

    function Számláló ( R: Racionális ) return Integer;
    function Nevezõ ( R: Racionális ) return Positive;

    function "/" ( Számláló: Integer; Nevezõ: Positive ) return Racionális;
    function "/" ( X, Y: Racionális ) return Racionális;
    function "/" ( X: Racionális; Y: Positive ) return Racionális;

    -- function "=" ( X, Y: Racionális ) return Boolean;

private

    type Racionális is record
                           Számláló: Integer := 0;
                           Nevezõ: Positive := 1;
                       end record;

end Racionalisok;
