generic
      
      type Elem is private;

package Vermek is

      type Verem (Max: Positive) is limited private;

      �res_A_Verem, Tele_A_Verem: exception;

      procedure Push( V: in out Verem; E: in Elem );
         -- kiv�lthat Tele_A_Verem kiv�telt

      procedure Pop( V: in out Verem; E: out Elem );
         -- kiv�lthat �res_A_Verem kiv�telt

      function Top( V: Verem ) return Elem;
         -- kiv�lthat �res_A_Verem kiv�telt

      function Is_Empty( V: Verem ) return Boolean;
      function Is_Full( V: Verem ) return Boolean;
      function Size( V: Verem ) return Natural;

private
      type T�mb is array( Integer range <> ) of Elem;
      type Verem(Max: Positive ) is record
                                        Adatok: T�mb(1..Max);
                                        Veremtet�: Natural := 0;
                                    end record;
end Vermek;

