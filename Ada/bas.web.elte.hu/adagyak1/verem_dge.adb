package body Verem_DGE is
   procedure Betev�s ( V : in out Verem; E : in Elem ) is
   begin
      V.Tartalom ( V.Mutat� + 1 ) := E;
      V.Mutat� := V.Mutat� + 1;
   exception
      when Constraint_Error =>
         raise Tele_Verem;
   end Betev�s;

   procedure Kidob�s ( V : in out Verem ) is
   begin
      V.Mutat� := V.Mutat� - 1;
   exception
      when Constraint_Error =>
         raise �res_Verem;
   end Kidob�s;

   function Tet� ( V : Verem ) return Elem is
   begin
      return V.Tartalom ( V.Mutat� );
   exception
      when Constraint_Error =>
         raise �res_Verem;
   end Tet�;

   function Tele ( V : Verem ) return Boolean is
   begin
      return V.Mutat� >= V.Max_M�ret;
   end Tele;

   function �res ( V : Verem ) return Boolean is
   begin
      return V.Mutat� <= 0;
   end �res;
end Verem_DGE;