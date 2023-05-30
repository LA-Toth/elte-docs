package body Vermek is

      procedure Push( V: in out Verem; E: in Elem ) is
      begin
          V.Veremtetõ := V.Veremtetõ + 1;
          V.Adatok(V.Veremtetõ) := E;
      end;

      procedure Pop( V: in out Verem; E: out Elem ) is
      begin
          E := Top(V);
          V.Veremtetõ := V.Veremtetõ - 1;
      end;

      function Top( V: Verem ) return Elem is
      begin
          return V.Adatok(V.Veremtetõ);
      end;

      function Is_Empty( V: Verem ) return Boolean is
      begin
          return V.Veremtetõ = 0;
      end;

      function Is_Full( V: Verem ) return Boolean is
      begin
          return V.Veremtetõ >= V.Max;
      end;

      function Size( V: Verem ) return Natural is
      begin
          return V.Veremtetõ;
      end;

end Vermek;

