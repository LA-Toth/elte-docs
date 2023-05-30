package body Verem_D is
   procedure Betevés ( V : in out Verem; E : in Elem ) is
   begin
      V.Mutató := V.Mutató + 1;
      V.Tartalom ( V.Mutató ) := E;
   end Betevés;

   procedure Kidobás ( V : in out Verem ) is
   begin
      V.Mutató := V.Mutató - 1;
   end Kidobás;

   function Tetõ ( V : Verem ) return Elem is
   begin
      return V.Tartalom ( V.Mutató );
   end Tetõ;

   function Tele ( V : Verem ) return Boolean is
   begin
      return V.Mutató >= V.Max_Méret;
   end Tele;

   function Üres ( V : Verem ) return Boolean is
   begin
      return V.Mutató <= 0;
   end Üres;
end Verem_D;                          