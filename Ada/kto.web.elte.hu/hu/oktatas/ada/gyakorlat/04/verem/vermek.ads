package Vermek is

      subtype Elem is Integer;

      type Verem (Max: Positive) is limited private;

      procedure Push( V: in out Verem; E: in Elem );
      procedure Pop( V: in out Verem; E: out Elem );
      function Top( V: Verem ) return Elem;
      function Is_Empty( V: Verem ) return Boolean;
      function Is_Full( V: Verem ) return Boolean;
      function Size( V: Verem ) return Natural;

private
      type Tömb is array( Integer range <> ) of Elem;
      type Verem(Max: Positive ) is record
                                        Adatok: Tömb(1..Max);
                                        Veremtetõ: Natural := 0;
                                    end record;
end Vermek;

