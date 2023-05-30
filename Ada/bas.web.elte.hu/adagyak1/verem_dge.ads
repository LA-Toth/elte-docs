generic
   type Elem is private;
   
   M�ret_Korl�t : in Natural := 1000;
   Alap_M�ret : in Natural := 100;

package Verem_DGE is
   subtype M�ret is Natural range 1 .. M�ret_Korl�t;
   type Verem ( Max_M�ret : M�ret := Alap_M�ret ) is limited private;

   procedure Betev�s ( V : in out Verem; E : in Elem );
   procedure Kidob�s ( V : in out Verem );
   function Tet� ( V : Verem ) return Elem;
   function Tele ( V : Verem ) return Boolean;
   function �res ( V : Verem ) return Boolean;
 
   Tele_Verem, �res_Verem : exception;

private
   type Vektor is array ( Positive range <> ) of Elem;
   type Verem ( Max_M�ret : M�ret := Alap_M�ret ) is
      record
         Mutat� : Integer range 0 .. M�ret_Korl�t := 0;
         Tartalom : Vektor ( 1 .. Max_M�ret );
      end record;
end Verem_DGE;