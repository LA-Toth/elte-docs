with Ada.Integer_Text_IO, Ada.Text_IO;
use Ada.Integer_Text_IO, Ada.Text_IO;

procedure Rendezes is

  type Cs�cs;
  type Mutat� is access Cs�cs;
  type Cs�cs is record
                   Adat: Integer;
                   K�vetkez�: Mutat�;
                end record;

  procedure Rendez ( Lista: in out Mutat� ) is
      V�ge: Mutat� := Lista;
      Tmp, Fut�: Mutat�;
  begin
      if Lista /= null and then Lista.K�vetkez� /= null then
          while V�ge.K�vetkez� /= null loop
             if Lista.Adat > V�ge.K�vetkez�.Adat then
                Tmp := V�ge.K�vetkez�;
                V�ge.K�vetkez� := Tmp.K�vetkez�;
                Tmp.K�vetkez� := Lista;
                Lista := Tmp;
             else
                Fut� := Lista;
                while Fut� /= V�ge and then Fut�.K�vetkez�.Adat <= V�ge.K�vetkez�.Adat loop
                         Fut� := Fut�.K�vetkez�;
                end loop;
                if Fut� /= V�ge then
                     Tmp := V�ge.K�vetkez�;
                     V�ge.K�vetkez� := Tmp.K�vetkez�;
                     Tmp.K�vetkez� := Fut�.K�vetkez�;
                     Fut�.K�vetkez� := Tmp;
                else
                     V�ge := V�ge.K�vetkez�;
                end if;
             end if;
          end loop;
      end if;
  end Rendez;

  procedure Ki�r ( Lista: in Mutat� ) is
     P: Mutat� := Lista;
  begin
     while P /= null loop
         Put(P.Adat);
         P := P.K�vetkez�;
     end loop;
     New_Line;
  end Ki�r;

  Lista: Mutat� := null;

begin

     declare
        N: Integer;
     begin
        loop
           Get(N);
           Lista := new Cs�cs'(N,Lista);
        end loop;
     exception
        when others => null;
     end;

     Ki�r(Lista);
     Rendez(Lista);
     Ki�r(Lista);

end Rendezes;

