with Ada.Integer_Text_IO, Ada.Text_IO;
use Ada.Integer_Text_IO, Ada.Text_IO;

procedure Rendezes is

  type Csúcs;
  type Mutató is access Csúcs;
  type Csúcs is record
                   Adat: Integer;
                   Következõ: Mutató;
                end record;

  procedure Beolvas ( Lista: out Mutató; F: in File_Type ) is
        N: Integer;
  begin
        Lista := null;
        loop
           declare
           begin
               Get(F,N);
               Lista := new Csúcs'(N,Lista);
           exception
               when Data_Error => Skip_Line(F);
           end;
        end loop;
  exception
        when End_Error => null;
  end Beolvas;
 
  procedure Rendez ( Lista: in out Mutató ) is
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
	  Lista := Beszúr(Lista.Következõ, Lista);
      end if;
  end Rendez;

  procedure Kiír ( Lista: in Mutató; F: in File_Type ) is
     P: Mutató := Lista;
  begin
     while P /= null loop
         Put(F,P.Adat);
         P := P.Következõ;
     end loop;
     New_Line(F);
  end Kiír;

  Lista: Mutató := null;

begin

     Beolvas(Lista, Current_Input);
     Kiír(Lista, Current_Output);
     Rendez(Lista);
     Kiír(Lista, Current_Output);

end Rendezes;

