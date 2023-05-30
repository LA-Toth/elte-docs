with Ada.Unchecked_Deallocation;

package body Vermek is

      procedure Felszabad�t is new Ada.Unchecked_Deallocation(Cs�cs, Mutat�);

      procedure Push( V: in out Verem; E: in Elem ) is
      begin
              V.Veremtet� := new Cs�cs'(E,V.Veremtet�);
	      V.M�ret := V.M�ret + 1;
      end;

      procedure Pop( V: in out Verem; E: out Elem ) is
          R�gi: Mutat� := V.Veremtet�;
      begin
          E := Top(V);
          V.Veremtet� := V.Veremtet�.K�vetkez�;
	  V.M�ret := V.M�ret - 1;
	  Felszabad�t(R�gi);
      end;

      function Top( V: Verem ) return Elem is
      begin
          return V.Veremtet�.Adat;
      exception
          when Constraint_Error => raise �res_A_Verem;
      end;

      function Is_Empty( V: Verem ) return Boolean is
      begin
          return V.Veremtet� = null;
      end;

      function Is_Full( V: Verem ) return Boolean is
      begin
          return false;
      end;

      function Size( V: Verem ) return Natural is
      begin
          return V.M�ret;
      end;

      procedure Finalize ( V: in out Verem ) is
          P: Mutat�;
      begin
          while V.Veremtet� /= null loop
               P := V.Veremtet�;
               V.Veremtet� := V.Veremtet�.K�vetkez�;
               Felszabad�t(P);
          end loop;
      end Finalize;
end Vermek;

