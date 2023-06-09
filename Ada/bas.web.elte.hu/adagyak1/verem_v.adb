package body Verem_V is
   procedure Betev�s ( V : in out Verem; E : in Elem ) is
   begin
      V.Mutat� := V.Mutat� + 1;
      V.Tartalom ( V.Mutat� ) := E;
   end Betev�s;

   procedure Kidob�s ( V : in out Verem ) is 
   begin
      V.Mutat� := V.Mutat� - 1;
   end Kidob�s;

   function Tet� ( V : Verem ) return Elem is
   begin
      return V.Tartalom ( V.Mutat� );
   end Tet�;

   function Tele ( V : Verem ) return Boolean is 
   begin
      return V.Mutat� = Max_M�ret;
   end;

   function �res ( V : Verem ) return Boolean is 
   begin
      return V.Mutat� <= 0;
   end;
end Verem_V;                          