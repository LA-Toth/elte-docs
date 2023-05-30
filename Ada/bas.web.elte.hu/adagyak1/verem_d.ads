package Verem_D is
   subtype Elem is Integer;
   subtype M�ret is Natural range 1 .. 1000;
   type Verem ( Max_M�ret : M�ret := 100 ) is limited private;

   procedure Betev�s ( V : in out Verem; E : in Elem );
   procedure Kidob�s ( V : in out Verem );
   function Tet� ( V : Verem ) return Elem;
   function Tele ( V : Verem ) return Boolean;
   function �res ( V : Verem ) return Boolean;

private
   type Vektor is array ( Positive range <> ) of Elem;
   type Verem ( Max_M�ret : M�ret := 100 ) is
      record
         Mutat� : Integer := 0;
         Tartalom : Vektor ( 1 .. Max_M�ret );
      end record;
end Verem_D;