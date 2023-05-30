generic
   type Elem is private;

package Verem_LGE is
   type Verem is limited private;

   procedure Betevés ( V : in out Verem; E : in Elem );
   procedure Kidobás ( V : in out Verem );
   function Tetõ ( V : Verem ) return Elem;
   function Üres ( V : Verem ) return Boolean;

   Üres_Verem : exception;

private
   type Doboz;
   type Doboz_Mutató is access Doboz;
   type Doboz is
      record
         Adat : Elem;
         Következõ : Doboz_Mutató := null;
      end record;
   type Verem is
      record
         Tetõ : Doboz_Mutató := null;
      end record;
end Verem_LGE;
