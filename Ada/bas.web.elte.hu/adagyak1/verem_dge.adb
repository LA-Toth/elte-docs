package body Verem_DGE is
   procedure Betevés ( V : in out Verem; E : in Elem ) is
   begin
      V.Tartalom ( V.Mutató + 1 ) := E;
      V.Mutató := V.Mutató + 1;
   exception
      when Constraint_Error =>
         raise Tele_Verem;
   end Betevés;

   procedure Kidobás ( V : in out Verem ) is
   begin
      V.Mutató := V.Mutató - 1;
   exception
      when Constraint_Error =>
         raise Üres_Verem;
   end Kidobás;

   function Tetõ ( V : Verem ) return Elem is
   begin
      return V.Tartalom ( V.Mutató );
   exception
      when Constraint_Error =>
         raise Üres_Verem;
   end Tetõ;

   function Tele ( V : Verem ) return Boolean is
   begin
      return V.Mutató >= V.Max_Méret;
   end Tele;

   function Üres ( V : Verem ) return Boolean is
   begin
      return V.Mutató <= 0;
   end Üres;
end Verem_DGE;