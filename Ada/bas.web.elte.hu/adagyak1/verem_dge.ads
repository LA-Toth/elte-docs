generic
   type Elem is private;
   
   Méret_Korlát : in Natural := 1000;
   Alap_Méret : in Natural := 100;

package Verem_DGE is
   subtype Méret is Natural range 1 .. Méret_Korlát;
   type Verem ( Max_Méret : Méret := Alap_Méret ) is limited private;

   procedure Betevés ( V : in out Verem; E : in Elem );
   procedure Kidobás ( V : in out Verem );
   function Tetõ ( V : Verem ) return Elem;
   function Tele ( V : Verem ) return Boolean;
   function Üres ( V : Verem ) return Boolean;
 
   Tele_Verem, Üres_Verem : exception;

private
   type Vektor is array ( Positive range <> ) of Elem;
   type Verem ( Max_Méret : Méret := Alap_Méret ) is
      record
         Mutató : Integer range 0 .. Méret_Korlát := 0;
         Tartalom : Vektor ( 1 .. Max_Méret );
      end record;
end Verem_DGE;