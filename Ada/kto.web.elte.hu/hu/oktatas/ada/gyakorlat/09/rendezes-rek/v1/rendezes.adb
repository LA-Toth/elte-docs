with Ada.Integer_Text_IO, Ada.Text_IO;
use Ada.Integer_Text_IO, Ada.Text_IO;

procedure Rendezes is

  type Cs�cs;
  type Mutat� is access Cs�cs;
  type Cs�cs is record
                   Adat: Integer;
                   K�vetkez�: Mutat�;
                end record;

  procedure Beolvas ( Lista: out Mutat� ) is
        N: Integer;
  begin
        Lista := null;
        loop
           Get(N);
           Lista := new Cs�cs'(N,Lista);
        end loop;
  exception
        when others => null;
  end Beolvas;
 
  procedure Rendez ( Lista: in out Mutat� ) is
      procedure Besz�r ( Hova, Mit: in Mutat�; Eredm�ny: out Mutat� ) is
      begin
          if Hova = null or else Mit.Adat <= Hova.Adat then
              Eredm�ny := Mit;
              Eredm�ny.K�vetkez� := Hova;
          else
              Besz�r(Hova.K�vetkez�, Mit, Eredm�ny);
              Hova.K�vetkez� := Eredm�ny;
              Eredm�ny := Hova;
          end if;
      end Besz�r;
      function Besz�r ( Hova, Mit: Mutat� ) return Mutat� is
      begin
          if Hova = null or else Mit.Adat <= Hova.Adat then
             Mit.K�vetkez� := Hova;
             return Mit;
          else
             Hova.K�vetkez� := Besz�r(Hova.K�vetkez�, Mit);
             return Hova;
          end if;
      end;
  begin
      if Lista /= null then
          Rendez(Lista.K�vetkez�);
          -- Besz�r(Lista.K�vetkez�, Lista, Lista);
	  Lista := Besz�r(Lista.K�vetkez�, Lista);
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

     Beolvas(Lista);
     Ki�r(Lista);
     Rendez(Lista);
     Ki�r(Lista);

end Rendezes;

