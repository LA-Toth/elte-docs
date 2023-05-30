generic
   type Elem is private;

   Méret_Korlát : in Natural := 1000;
   Alap_Méret : in Natural := 100;

package Sor_DG is
   subtype Méret is Natural range 1 .. Méret_Korlát;
   type Sor ( Max_Méret : Méret := Alap_Méret ) is limited private; 

   procedure Betevés ( S : in out Sor; E : in Elem );
   procedure Kidobás ( S : in out Sor );
   function Elsõ ( S : Sor ) return Elem; 
   function Tele ( S : Sor ) return Boolean; 
   function Üres ( S : Sor ) return Boolean; 

private

   type Vektor is array ( Positive range <> ) of Elem; 
   type Sor ( Max_Méret : Méret := Alap_Méret ) is 
      record
         Vége : Integer := 0;
         Eleje : Integer := 1;
         Tartalom : Vektor ( 1 .. Max_Méret );  
      end record; 
end Sor_DG;