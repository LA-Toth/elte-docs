function Talalt ( Szoveg : String; Ch : Character; Poz : Natural )
   return Boolean is
begin
   return ( Szoveg ( Poz + 1 ) = Ch );
end Talalt;    