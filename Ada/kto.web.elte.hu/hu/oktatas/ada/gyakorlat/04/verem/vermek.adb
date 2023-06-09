package body Vermek is

      procedure Push( V: in out Verem; E: in Elem ) is
      begin
          V.Veremtető := V.Veremtető + 1;
          V.Adatok(V.Veremtető) := E;
      end;

      procedure Pop( V: in out Verem; E: out Elem ) is
      begin
          E := Top(V);
          V.Veremtető := V.Veremtető - 1;
      end;

      function Top( V: Verem ) return Elem is
      begin
          return V.Adatok(V.Veremtető);
      end;

      function Is_Empty( V: Verem ) return Boolean is
      begin
          return V.Veremtető = 0;
      end;

      function Is_Full( V: Verem ) return Boolean is
      begin
          return V.Veremtető >= V.Max;
      end;

      function Size( V: Verem ) return Natural is
      begin
          return V.Veremtető;
      end;

end Vermek;

