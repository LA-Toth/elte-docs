generic
   type Elem is private;

package Verem_LGE is
   type Verem is limited private;

   procedure Betev�s ( V : in out Verem; E : in Elem );
   procedure Kidob�s ( V : in out Verem );
   function Tet� ( V : Verem ) return Elem;
   function �res ( V : Verem ) return Boolean;

   �res_Verem : exception;

private
   type Doboz;
   type Doboz_Mutat� is access Doboz;
   type Doboz is
      record
         Adat : Elem;
         K�vetkez� : Doboz_Mutat� := null;
      end record;
   type Verem is
      record
         Tet� : Doboz_Mutat� := null;
      end record;
end Verem_LGE;
