package body Verem_LGE is
   procedure Betevés ( V : in out Verem; E : in Elem ) is
   begin
      V.Tetõ := new Doboz' ( E, V.Tetõ );
   end Betevés;

   procedure Kidobás ( V : in out Verem ) is
   begin
      V.Tetõ := V.Tetõ.Következõ;
   exception
      when Constraint_Error =>
         raise Üres_Verem;
   end Kidobás;

   function Tetõ ( V : Verem ) return Elem is
   begin
      return V.Tetõ.Adat;
   exception
      when Constraint_Error =>
         raise Üres_Verem;
   end Tetõ;

   function Üres ( V : Verem ) return Boolean is 
   begin
      return ( V.Tetõ = null );
   end Üres;

end Verem_LGE;