with Ada.Finalization; use Ada.Finalization;

generic
      
      type Elem is private;

package Vermek is

      type Verem is new Limited_Controlled with private;

      Üres_A_Verem: exception;

      procedure Push( V: in out Verem; E: in Elem );

      procedure Pop( V: in out Verem; E: out Elem );
         -- kiválthat Üres_A_Verem kivételt

      function Top( V: Verem ) return Elem;
         -- kiválthat Üres_A_Verem kivételt

      function Is_Empty( V: Verem ) return Boolean;
      function Is_Full( V: Verem ) return Boolean;
      function Size( V: Verem ) return Natural;

      procedure Finalize( V: in out Verem );

private
      type Csúcs;
      type Mutató is access Csúcs;
      type Csúcs is record
                        Adat: Elem;
                        Következõ: Mutató;
                    end record;
      type Verem is new Limited_Controlled with record
                                        Méret: Natural := 0;
                                        Veremtetõ: Mutató := null;
                                    end record;
end Vermek;

