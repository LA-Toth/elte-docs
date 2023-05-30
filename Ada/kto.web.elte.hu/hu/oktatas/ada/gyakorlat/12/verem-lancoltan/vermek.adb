with Ada.Unchecked_Deallocation;

package body Vermek is

   procedure Push( V: in out Verem; E: in Elem ) is
   begin
       V.Veremteto := new Csucs'(E,V.Veremteto);
       V.Meret := V.Meret + 1;
   end;

   procedure Pop( V: in out Verem; E: out Elem ) is
       procedure Free is new Ada.Unchecked_Deallocation(Csucs,Mutato);
       Torlendo: Mutato := V.Veremteto;
   begin
       if V.Veremteto /= null then
          E := V.Veremteto.Adat;
	  --V := (Controlled(V) with Veremteto=>V.Veremteto.Kovetkezo, Meret=>V.Meret - 1);
	  V.Veremteto := V.Veremteto.Kovetkezo;
	  V.Meret := V.Meret - 1;
	  V := (Controlled(V) with Veremteto=>V.Veremteto.Kovetkezo, Meret=>V.Meret - 1);
	  Free(Torlendo);
       else
          raise Ures_Verem;
       end if;
   end;

   procedure Pop( V: in out Verem ) is
       E: Elem;
   begin
       Pop(V,E);
       -- nem használom E-t
   end;

   function Top( V: Verem ) return Elem is
   begin
       if V.Meret /= 0 then
          return V.Veremteto.Adat;
       else
          raise Ures_Verem;
       end if;
   end;

   function Is_Empty( V: Verem ) return Boolean is
   begin
       return V.Meret = 0;
   end;

   function Size( V: Verem ) return Natural is
   begin
       return V.Meret;
   end;

   procedure Apply_On_All_Elements( V: in Verem ) is
       P: Mutato := V.Veremteto;
   begin
       while P /= null loop
          Action(P.Adat);
	  P := P.Kovetkezo;
       end loop;
   end;

   function "="( V1, V2: Verem ) return Boolean is     -- Házi feladat
   begin
       return False;
   end;

   procedure Adjust( V: in out Verem ) is              -- Házi feladat
   begin
       null;
   end;

   procedure Finalize( V: in out Verem ) is            -- Házi feladat
   begin
       null;
   end;

end Vermek;
