package Verem_V is
   subtype Elem is Integer;
   type Verem is limited private; 

   procedure Betevés ( V : in out Verem; E : in Elem );
   procedure Kidobás ( V : in out Verem );
   function Tetõ ( V : Verem ) return Elem; 
   function Tele ( V : Verem ) return Boolean; 
   function Üres ( V : Verem ) return Boolean; 

private

   Max_Méret : constant := 100;

   type Vektor is array ( 1 .. Max_Méret ) of Elem; 
   type Verem is 
      record 
         Mutató : Integer := 0;  
         Tartalom : Vektor;  
      end record; 
end Verem_V;