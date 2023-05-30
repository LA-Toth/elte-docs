generic
   type Elem is limited private;

   with function Vege ( E : Elem ) return Boolean;
   with procedure Atalakit ( E : in out Elem );

procedure Iteral ( E : in out Elem; Lepes_Szam : out Natural );