with Ada.Integer_Text_IO, Ada.Text_IO;
use Ada.Integer_Text_IO, Ada.Text_IO;

procedure Rendezes is

  type Csúcs;
  type Mutató is access Csúcs;
  type Csúcs is record
                   Adat: Integer;
                   Következõ: Mutató;
                end record;

  procedure Rendez ( Lista: in out Mutató ) is
      Vége: Mutató := Lista;
      Tmp, Futó: Mutató;
  begin
      if Lista /= null and then Lista.Következõ /= null then
          while Vége.Következõ /= null loop
             if Lista.Adat > Vége.Következõ.Adat then
                Tmp := Vége.Következõ;
                Vége.Következõ := Tmp.Következõ;
                Tmp.Következõ := Lista;
                Lista := Tmp;
             else
                Futó := Lista;
                while Futó /= Vége and then Futó.Következõ.Adat <= Vége.Következõ.Adat loop
                         Futó := Futó.Következõ;
                end loop;
                if Futó /= Vége then
                     Tmp := Vége.Következõ;
                     Vége.Következõ := Tmp.Következõ;
                     Tmp.Következõ := Futó.Következõ;
                     Futó.Következõ := Tmp;
                else
                     Vége := Vége.Következõ;
                end if;
             end if;
          end loop;
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

     declare
        N: Integer;
     begin
        loop
           Get(N);
           Lista := new Csúcs'(N,Lista);
        end loop;
     exception
        when others => null;
     end;

     Kiír(Lista);
     Rendez(Lista);
     Kiír(Lista);

end Rendezes;

