with Lnko;

package body Racionalisok is

    function Sz�ml�l� ( R: Racion�lis ) return Integer is
    begin
        return R.Sz�ml�l�;
    end;

    function Nevez� ( R: Racion�lis ) return Positive is
    begin
        return R.Nevez�;
    end;

    function Normaliz�lva ( Sz�ml�l�: Integer; Nevez�: Positive )
    return Racion�lis is
    begin
        if Sz�ml�l� = 0 then
            return (0,1);
        else
            declare
                H: Positive := Lnko( abs Sz�ml�l�, Nevez� );
            begin
                return (Sz�ml�l�/H, Nevez�/H);
            end;
        end if;
    end;

    function "/" ( Sz�ml�l�: Integer; Nevez�: Positive ) return Racion�lis is
    begin
        return Normaliz�lva( Sz�ml�l�, Nevez� );
    end;

    function "/" ( X, Y: Racion�lis ) return Racion�lis is
    begin
        return Normaliz�lva( X.Sz�ml�l� * Y.Nevez�,  X.Nevez� * Y.Sz�ml�l� );
    end;

    function "/" ( X: Racion�lis; Y: Positive ) return Racion�lis is
    begin
        return Normaliz�lva( X.Sz�ml�l�,  X.Nevez� * Y );
    end;

    -- function "=" ( X, Y: Racion�lis ) return Boolean is
    -- begin
    --     return X.Sz�ml�l� * Y.Nevez� = Y.Sz�ml�l� * X.Nevez�;
    -- end;

end Racionalisok;

