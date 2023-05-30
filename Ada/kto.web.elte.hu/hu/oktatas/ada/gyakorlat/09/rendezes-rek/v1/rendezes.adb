with Ada.Integer_Text_IO, Ada.Text_IO;
use Ada.Integer_Text_IO, Ada.Text_IO;

procedure Rendezes is

  type Csúcs;
  type Mutató is access Csúcs;
  type Csúcs is record
                   Adat: Integer;
                   Következõ: Mutató;
                end record;

  procedure Beolvas ( Lista: out Mutató ) is
        N: Integer;
  begin
        Lista := null;
        loop
           Get(N);
           Lista := new Csúcs'(N,Lista);
        end loop;
  exception
        when others => null;
  end Beolvas;
 
  procedure Rendez ( Lista: in out Mutató ) is
      procedure Beszúr ( Hova, Mit: in Mutató; Eredmény: out Mutató ) is
      begin
          if Hova = null or else Mit.Adat <= Hova.Adat then
              Eredmény := Mit;
              Eredmény.Következõ := Hova;
          else
              Beszúr(Hova.Következõ, Mit, Eredmény);
              Hova.Következõ := Eredmény;
              Eredmény := Hova;
          end if;
      end Beszúr;
      function Beszúr ( Hova, Mit: Mutató ) return Mutató is
      begin
          if Hova = null or else Mit.Adat <= Hova.Adat then
             Mit.Következõ := Hova;
             return Mit;
          else
             Hova.Következõ := Beszúr(Hova.Következõ, Mit);
             return Hova;
          end if;
      end;
  begin
      if Lista /= null then
          Rendez(Lista.Következõ);
          -- Beszúr(Lista.Következõ, Lista, Lista);
	  Lista := Beszúr(Lista.Következõ, Lista);
      end if;
  end Rendez;

  procedure Kiír ( Lista: in Mutató ) is
     P: Mutató := Lista;
  begin
     while P /= null loop
         Put(P.Adat);
         P := P.Következõ;
     end loop;
     New_Line;
  end Kiír;

  Lista: Mutató := null;

begin

     Beolvas(Lista);
     Kiír(Lista);
     Rendez(Lista);
     Kiír(Lista);

end Rendezes;

