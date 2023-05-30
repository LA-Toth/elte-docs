package Sor_D is
   subtype Elem is Integer;
   subtype Méret is Natural range 1 .. 1000;
   type Sor ( Max_Méret : Méret := 100 ) is limited private; 

   procedure Betevés ( S : in out Sor; E : in Elem );
   procedure Kidobás ( S : in out Sor );
   function Elsõ ( S : Sor ) return Elem; 
   function Tele ( S : Sor ) return Boolean; 
   function Üres ( S : Sor ) return Boolean; 

private

   type Vektor is array ( Positive range <> ) of Elem; 
   type Sor ( Max_Méret : Méret := 100 ) is 
      record
         Vége : Integer := 0;
         Eleje : Integer := 1;
         Tartalom : Vektor ( 1 .. Max_Méret );  
      end record; 
end Sor_D;