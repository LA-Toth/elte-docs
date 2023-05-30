package Racionalisok is

    type Racion�lis is private;

    function Sz�ml�l� ( R: Racion�lis ) return Integer;
    function Nevez� ( R: Racion�lis ) return Positive;

    function "/" ( Sz�ml�l�: Integer; Nevez�: Positive ) return Racion�lis;
    function "/" ( X, Y: Racion�lis ) return Racion�lis;
    function "/" ( X: Racion�lis; Y: Positive ) return Racion�lis;

    -- function "=" ( X, Y: Racion�lis ) return Boolean;

private

    type Racion�lis is record
                           Sz�ml�l�: Integer := 0;
                           Nevez�: Positive := 1;
                       end record;

end Racionalisok;
