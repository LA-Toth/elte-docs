generic
   type Elem is private;

   M�ret_Korl�t : in Natural := 1000;
   Alap_M�ret : in Natural := 100;

package Sor_DG is
   subtype M�ret is Natural range 1 .. M�ret_Korl�t;
   type Sor ( Max_M�ret : M�ret := Alap_M�ret ) is limited private; 

   procedure Betev�s ( S : in out Sor; E : in Elem );
   procedure Kidob�s ( S : in out Sor );
   function Els� ( S : Sor ) return Elem; 
   function Tele ( S : Sor ) return Boolean; 
   function �res ( S : Sor ) return Boolean; 

private

   type Vektor is array ( Positive range <> ) of Elem; 
   type Sor ( Max_M�ret : M�ret := Alap_M�ret ) is 
      record
         V�ge : Integer := 0;
         Eleje : Integer := 1;
         Tartalom : Vektor ( 1 .. Max_M�ret );  
      end record; 
end Sor_DG;