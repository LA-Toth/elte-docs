with Lnko;

package body Racionalisok is

    function Számláló ( R: Racionális ) return Integer is
    begin
        return R.Számláló;
    end;

    function Nevezõ ( R: Racionális ) return Positive is
    begin
        return R.Nevezõ;
    end;

    function Normalizálva ( Számláló: Integer; Nevezõ: Positive )
    return Racionális is
    begin
        if Számláló = 0 then
            return (0,1);
        else
            declare
                H: Positive := Lnko( abs Számláló, Nevezõ );
            begin
                return (Számláló/H, Nevezõ/H);
            end;
        end if;
    end;

    function "/" ( Számláló: Integer; Nevezõ: Positive ) return Racionális is
    begin
        return Normalizálva( Számláló, Nevezõ );
    end;

    function "/" ( X, Y: Racionális ) return Racionális is
    begin
        return Normalizálva( X.Számláló * Y.Nevezõ,  X.Nevezõ * Y.Számláló );
    end;

    function "/" ( X: Racionális; Y: Positive ) return Racionális is
    begin
        return Normalizálva( X.Számláló,  X.Nevezõ * Y );
    end;

    -- function "=" ( X, Y: Racionális ) return Boolean is
    -- begin
    --     return X.Számláló * Y.Nevezõ = Y.Számláló * X.Nevezõ;
    -- end;

end Racionalisok;

