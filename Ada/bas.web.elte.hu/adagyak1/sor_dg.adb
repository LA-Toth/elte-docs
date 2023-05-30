package body Sor_DG is
   procedure Betev�s ( S : in out Sor; E : in Elem ) is
   begin
      S.V�ge := S.V�ge + 1;
      if S.V�ge > S.Max_M�ret then
         S.V�ge := 1;
      end if;
      S.Tartalom ( S.V�ge ) := E;
   end Betev�s;

   procedure Kidob�s ( S : in out Sor ) is 
   begin
      S.Eleje := S.Eleje + 1;
      if S.Eleje = S.V�ge + 1 then
         S.V�ge := 0;
         S.Eleje := 1;
      elsif S.Eleje > S.Max_M�ret then
         S.Eleje := 1;
      end if;
   end Kidob�s;

   function Els� ( S : Sor ) return Elem is
   begin
      return S.Tartalom ( S.Eleje );
   end Els�;

   function Tele ( S : Sor ) return Boolean is 
   begin
      return ( S.V�ge + 1 = S.Eleje ) or else
             ( S.Eleje = 1 and then S.V�ge = S.Max_M�ret );
   end;

   function �res ( S : Sor ) return Boolean is 
   begin
      return S.V�ge = 0 and then S.Eleje = 1;
   end;
end Sor_DG;