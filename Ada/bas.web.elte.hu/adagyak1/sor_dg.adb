package body Sor_DG is
   procedure Betevés ( S : in out Sor; E : in Elem ) is
   begin
      S.Vége := S.Vége + 1;
      if S.Vége > S.Max_Méret then
         S.Vége := 1;
      end if;
      S.Tartalom ( S.Vége ) := E;
   end Betevés;

   procedure Kidobás ( S : in out Sor ) is 
   begin
      S.Eleje := S.Eleje + 1;
      if S.Eleje = S.Vége + 1 then
         S.Vége := 0;
         S.Eleje := 1;
      elsif S.Eleje > S.Max_Méret then
         S.Eleje := 1;
      end if;
   end Kidobás;

   function Elsõ ( S : Sor ) return Elem is
   begin
      return S.Tartalom ( S.Eleje );
   end Elsõ;

   function Tele ( S : Sor ) return Boolean is 
   begin
      return ( S.Vége + 1 = S.Eleje ) or else
             ( S.Eleje = 1 and then S.Vége = S.Max_Méret );
   end;

   function Üres ( S : Sor ) return Boolean is 
   begin
      return S.Vége = 0 and then S.Eleje = 1;
   end;
end Sor_DG;