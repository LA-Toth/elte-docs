package body Verem_LGE is
   procedure Betev�s ( V : in out Verem; E : in Elem ) is
   begin
      V.Tet� := new Doboz' ( E, V.Tet� );
   end Betev�s;

   procedure Kidob�s ( V : in out Verem ) is
   begin
      V.Tet� := V.Tet�.K�vetkez�;
   exception
      when Constraint_Error =>
         raise �res_Verem;
   end Kidob�s;

   function Tet� ( V : Verem ) return Elem is
   begin
      return V.Tet�.Adat;
   exception
      when Constraint_Error =>
         raise �res_Verem;
   end Tet�;

   function �res ( V : Verem ) return Boolean is 
   begin
      return ( V.Tet� = null );
   end �res;

end Verem_LGE;