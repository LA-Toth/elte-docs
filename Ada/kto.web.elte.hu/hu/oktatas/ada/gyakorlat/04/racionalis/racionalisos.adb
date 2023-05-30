with Racionalisok, Ada.Integer_Text_IO, Ada.Text_IO;
use Racionalisok, Ada.Integer_Text_IO, Ada.Text_IO;

procedure Racionalisos is

    R: Racionális := 4/8;
    -- X: Racionális := 3/4/5;

begin

    R := R / (R/2);
    Put( Számláló(R) );
    Put( '/' );
    Put( Nevezõ(R) );

end;
