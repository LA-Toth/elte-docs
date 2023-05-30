with Ada.Finalization; use Ada.Finalization;

generic
      
      type Elem is private;

package Vermek is

      type Verem is new Limited_Controlled with private;

      �res_A_Verem: exception;

      procedure Push( V: in out Verem; E: in Elem );

      procedure Pop( V: in out Verem; E: out Elem );
         -- kiv�lthat �res_A_Verem kiv�telt

      function Top( V: Verem ) return Elem;
         -- kiv�lthat �res_A_Verem kiv�telt

      function Is_Empty( V: Verem ) return Boolean;
      function Is_Full( V: Verem ) return Boolean;
      function Size( V: Verem ) return Natural;

      procedure Finalize( V: in out Verem );

private
      type Cs�cs;
      type Mutat� is access Cs�cs;
      type Cs�cs is record
                        Adat: Elem;
                        K�vetkez�: Mutat�;
                    end record;
      type Verem is new Limited_Controlled with record
                                        M�ret: Natural := 0;
                                        Veremtet�: Mutat� := null;
                                    end record;
end Vermek;

