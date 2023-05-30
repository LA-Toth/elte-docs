generic
      
      type Elem is private;

package Vermek is

      type Verem (Max: Positive) is limited private;

      Üres_A_Verem, Tele_A_Verem: exception;

      procedure Push( V: in out Verem; E: in Elem );
         -- kiválthat Tele_A_Verem kivételt

      procedure Pop( V: in out Verem; E: out Elem );
         -- kiválthat Üres_A_Verem kivételt

      function Top( V: Verem ) return Elem;
         -- kiválthat Üres_A_Verem kivételt

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

