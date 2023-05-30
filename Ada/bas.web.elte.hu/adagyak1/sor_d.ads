package Sor_D is
   subtype Elem is Integer;
   subtype M�ret is Natural range 1 .. 1000;
   type Sor ( Max_M�ret : M�ret := 100 ) is limited private; 

   procedure Betev�s ( S : in out Sor; E : in Elem );
   procedure Kidob�s ( S : in out Sor );
   function Els� ( S : Sor ) return Elem; 
   function Tele ( S : Sor ) return Boolean; 
   function �res ( S : Sor ) return Boolean; 

private

   type Vektor is array ( Positive range <> ) of Elem; 
   type Sor ( Max_M�ret : M�ret := 100 ) is 
      record
         V�ge : Integer := 0;
         Eleje : Integer := 1;
         Tartalom : Vektor ( 1 .. Max_M�ret );  
      end record; 
end Sor_D;