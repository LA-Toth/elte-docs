package Verem_D is
   subtype Elem is Integer;
   subtype Méret is Natural range 1 .. 1000;
   type Verem ( Max_Méret : Méret := 100 ) is limited private;

   procedure Betevés ( V : in out Verem; E : in Elem );
   procedure Kidobás ( V : in out Verem );
   function Tetõ ( V : Verem ) return Elem;
   function Tele ( V : Verem ) return Boolean;
   function Üres ( V : Verem ) return Boolean;

private
   type Vektor is array ( Positive range <> ) of Elem;
   type Verem ( Max_Méret : Méret := 100 ) is
      record
         Mutató : Integer := 0;
         Tartalom : Vektor ( 1 .. Max_Méret );
      end record;
end Verem_D;