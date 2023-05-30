with Ada.Integer_Text_IO, Ada.Text_IO, Ada.Command_Line;
use Ada.Integer_Text_IO, Ada.Text_IO, Ada.Command_Line;

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
  F_In, F_Out: File_Type;

begin
   if Argument_Count = 2 then
      Open(F_In, Name => Argument(1), Mode => In_File);
      declare begin
          Beolvas(Lista, F_In);
      exception
          when others => Close(F_In); raise;
      end;
      Close(F_In);
      Kiír(Lista, Current_Output);
      Rendez(Lista);
      Create(F_Out, Name => Argument(2));
      declare begin
          Kiír(Lista, F_Out);
      exception
          when others => Close(F_Out); raise;
      end;
      Close(F_Out);
  else 
      Put_Line(Current_Error, "Argumentum: a bemenet és a kimenet fájl neve.");
  end if;
end Rendezes;

