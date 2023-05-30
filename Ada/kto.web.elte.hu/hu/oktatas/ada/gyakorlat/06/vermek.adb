package body Vermek is

      procedure Push( V: in out Verem; E: in Elem ) is
      begin
          V.Veremtet� := new Cs�cs'(E,V.Veremtet�);
          V.M�ret := V.M�ret + 1;
      end;

      procedure Pop( V: in out Verem; E: out Elem ) is
      begin
          E := Top(V);
          V.Veremtet� := V.Veremtet�.K�vetkez�;
          V.M�ret := V.M�ret - 1;
      end;

      function Top( V: Verem ) return Elem is
      begin
          return V.Veremtet�.Adat;
      exception
          when Constraint_Error => raise �res_A_Verem;
      end;

      function Is_Empty( V: Verem ) return Boolean is
      begin
          return V.M�ret = 0;
      end;

      function Is_Full( V: Verem ) return Boolean is
      begin
          return False;
      end;

      function Size( V: Verem ) return Natural is
      begin
          return V.M�ret;
      end;

end Vermek;

