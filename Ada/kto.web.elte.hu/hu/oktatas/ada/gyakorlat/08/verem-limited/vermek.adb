with Ada.Unchecked_Deallocation;

package body Vermek is

      procedure Felszabadít is new Ada.Unchecked_Deallocation(Csúcs, Mutató);

      procedure Push( V: in out Verem; E: in Elem ) is
      begin
              V.Veremtetõ := new Csúcs'(E,V.Veremtetõ);
	      V.Méret := V.Méret + 1;
      end;

      procedure Pop( V: in out Verem; E: out Elem ) is
          Régi: Mutató := V.Veremtetõ;
      begin
          E := Top(V);
          V.Veremtetõ := V.Veremtetõ.Következõ;
	  V.Méret := V.Méret - 1;
	  Felszabadít(Régi);
      end;

      function Top( V: Verem ) return Elem is
      begin
          return V.Veremtetõ.Adat;
      exception
          when Constraint_Error => raise Üres_A_Verem;
      end;

      function Is_Empty( V: Verem ) return Boolean is
      begin
          return V.Veremtetõ = null;
      end;

      function Is_Full( V: Verem ) return Boolean is
      begin
          return false;
      end;

      function Size( V: Verem ) return Natural is
      begin
          return V.Méret;
      end;

      procedure Finalize ( V: in out Verem ) is
          P: Mutató;
      begin
          while V.Veremtetõ /= null loop
               P := V.Veremtetõ;
               V.Veremtetõ := V.Veremtetõ.Következõ;
               Felszabadít(P);
          end loop;
      end Finalize;
end Vermek;

