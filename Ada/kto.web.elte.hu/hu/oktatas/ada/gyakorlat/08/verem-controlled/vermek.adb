with Ada.Unchecked_Deallocation;

with Ada.Text_IO;  -- nyomkövetéshez

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
          Ada.Text_IO.Put_Line("Finalize");    -- nyomkövetés
          while V.Veremtetõ /= null loop
               P := V.Veremtetõ;
               V.Veremtetõ := V.Veremtetõ.Következõ;
               Felszabadít(P);
          end loop;
      end Finalize;

--      procedure Adjust( V: in out Verem ) is
--          Eredeti: Mutató := V.Veremtetõ;
--          Másolat: Mutató;
--      begin
--          if Eredeti /= null then
--              Másolat := new Csúcs'(Eredeti.Adat, null);
--              V.Veremtetõ := Másolat;
--              while Eredeti.Következõ /= null loop
--                   Eredeti := Eredeti.Következõ;
--                   Másolat.Következõ := new Csúcs'(Eredeti.Adat, null);
--                   Másolat := Másolat.Következõ;
--              end loop;
--          end if;
--      end Adjust;

      procedure Adjust( V: in out Verem ) is
          P: Mutató := V.Veremtetõ;
      begin
          Ada.Text_IO.Put_Line("Adjust");      -- nyomkövetés
          if P /= null then
              P := new Csúcs'(P.all);
              V.Veremtetõ := P;
              while P.Következõ /= null loop
                   P.Következõ := new Csúcs'(P.Következõ.all);
                   P := P.Következõ;
              end loop;
          end if;
      end Adjust;

      function "=" ( A, B: Verem ) return Boolean is
          P: Mutató := A.Veremtetõ;
          Q: Mutató := B.Veremtetõ;
      begin
          if A.Méret /= B.Méret then return false; end if;
          while P /= null loop    -- akkor és csak akkor, ha Q sem null
              if P.Adat /= Q.Adat then return false; end if;
              P := P.Következõ;
              Q := Q.Következõ;
          end loop;
          return true;
      end "=";

end Vermek;

