with Racionalisok, Ada.Integer_Text_IO, Ada.Text_IO;
use Racionalisok, Ada.Integer_Text_IO, Ada.Text_IO;

procedure Racionalisos is

    R: Racion�lis := 4/8;
    -- X: Racion�lis := 3/4/5;

begin

    R := R / (R/2);
    Put( Sz�ml�l�(R) );
    Put( '/' );
    Put( Nevez�(R) );

end;
