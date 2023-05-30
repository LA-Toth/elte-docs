with Ada.Unchecked_Deallocation;

with Ada.Text_IO;  -- nyomk�vet�shez

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
          Ada.Text_IO.Put_Line("Finalize");    -- nyomk�vet�s
          while V.Veremtet� /= null loop
               P := V.Veremtet�;
               V.Veremtet� := V.Veremtet�.K�vetkez�;
               Felszabad�t(P);
          end loop;
      end Finalize;

--      procedure Adjust( V: in out Verem ) is
--          Eredeti: Mutat� := V.Veremtet�;
--          M�solat: Mutat�;
--      begin
--          if Eredeti /= null then
--              M�solat := new Cs�cs'(Eredeti.Adat, null);
--              V.Veremtet� := M�solat;
--              while Eredeti.K�vetkez� /= null loop
--                   Eredeti := Eredeti.K�vetkez�;
--                   M�solat.K�vetkez� := new Cs�cs'(Eredeti.Adat, null);
--                   M�solat := M�solat.K�vetkez�;
--              end loop;
--          end if;
--      end Adjust;

      procedure Adjust( V: in out Verem ) is
          P: Mutat� := V.Veremtet�;
      begin
          Ada.Text_IO.Put_Line("Adjust");      -- nyomk�vet�s
          if P /= null then
              P := new Cs�cs'(P.all);
              V.Veremtet� := P;
              while P.K�vetkez� /= null loop
                   P.K�vetkez� := new Cs�cs'(P.K�vetkez�.all);
                   P := P.K�vetkez�;
              end loop;
          end if;
      end Adjust;

      function "=" ( A, B: Verem ) return Boolean is
          P: Mutat� := A.Veremtet�;
          Q: Mutat� := B.Veremtet�;
      begin
          if A.M�ret /= B.M�ret then return false; end if;
          while P /= null loop    -- akkor �s csak akkor, ha Q sem null
              if P.Adat /= Q.Adat then return false; end if;
              P := P.K�vetkez�;
              Q := Q.K�vetkez�;
          end loop;
          return true;
      end "=";

end Vermek;

