package Verem_V is
   subtype Elem is Integer;
   type Verem is limited private; 

   procedure Betev�s ( V : in out Verem; E : in Elem );
   procedure Kidob�s ( V : in out Verem );
   function Tet� ( V : Verem ) return Elem; 
   function Tele ( V : Verem ) return Boolean; 
   function �res ( V : Verem ) return Boolean; 

private

   Max_M�ret : constant := 100;

   type Vektor is array ( 1 .. Max_M�ret ) of Elem; 
   type Verem is 
      record 
         Mutat� : Integer := 0;  
         Tartalom : Vektor;  
      end record; 
end Verem_V;