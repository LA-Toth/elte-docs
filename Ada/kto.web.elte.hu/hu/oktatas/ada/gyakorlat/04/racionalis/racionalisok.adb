with Lnko;

package body Racionalisok is

    function Számláló ( R: Racionális ) return Integer is
    begin
        return R.Számláló;
    end;

    function Nevező ( R: Racionális ) return Positive is
    begin
        return R.Nevező;
    end;

    function Normalizálva ( Számláló: Integer; Nevező: Positive )
    return Racionális is
    begin
        if Számláló = 0 then
            return (0,1);
        else
            declare
                H: Positive := Lnko( abs Számláló, Nevező );
            begin
                return (Számláló/H, Nevező/H);
            end;
        end if;
    end;

    function "/" ( Számláló: Integer; Nevező: Positive ) return Racionális is
    begin
        return Normalizálva( Számláló, Nevező );
    end;

    function "/" ( X, Y: Racionális ) return Racionális is
    begin
        return Normalizálva( X.Számláló * Y.Nevező,  X.Nevező * Y.Számláló );
    end;

    function "/" ( X: Racionális; Y: Positive ) return Racionális is
    begin
        return Normalizálva( X.Számláló,  X.Nevező * Y );
    end;

    -- function "=" ( X, Y: Racionális ) return Boolean is
    -- begin
    --     return X.Számláló * Y.Nevező = Y.Számláló * X.Nevező;
    -- end;

end Racionalisok;

