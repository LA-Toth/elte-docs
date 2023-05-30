with Ada.Sequential_IO, Ada.Integer_Text_IO, Ada.Text_IO, Ada.Command_Line;
use Ada.Integer_Text_IO, Ada.Text_IO, Ada.Command_Line;

procedure Rendezes is

  package Int_Seq_IO is new Ada.Sequential_IO(Integer);
  use Int_Seq_IO;
	
  type Cs�cs;
  type Mutat� is access Cs�cs;
  type Cs�cs is record
                   Adat: Integer;
                   K�vetkez�: Mutat�;
                end record;

  procedure Beolvas ( Lista: out Mutat�; F: in Ada.Text_IO.File_Type ) is
        N: Integer;
  begin
        Lista := null;
        loop
           declare
           begin
               Get(F,N);
               Lista := new Cs�cs'(N,Lista);
           exception
               when Ada.Text_IO.Data_Error => Skip_Line(F);
           end;
        end loop;
  exception
        when Ada.Text_IO.End_Error => null;
  end Beolvas;
 
  procedure Rendez ( Lista: in out Mutat� ) is
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
	  Lista := Besz�r(Lista.K�vetkez�, Lista);
      end if;
  end Rendez;

  procedure Ki�r ( Lista: in Mutat�; F: in Int_Seq_IO.File_Type ) is
     P: Mutat� := Lista;
  begin
     while P /= null loop
         Write(F,P.Adat);
         P := P.K�vetkez�;
     end loop;
  end Ki�r;

  Lista: Mutat� := null;
  F_In: Ada.Text_IO.File_Type;
  F_Out: Int_Seq_IO.File_Type;

begin
   if Argument_Count = 2 then
      Open(F_In, Name => Argument(1), Mode => In_File);
      declare begin
          Beolvas(Lista, F_In);
      exception
          when others => Close(F_In); raise;
      end;
      Close(F_In);
      Rendez(Lista);
      Create(F_Out, Name => Argument(2));
      declare begin
          Ki�r(Lista, F_Out);
      exception
          when others => Close(F_Out); raise;
      end;
      Close(F_Out);
  else 
      Put_Line(Current_Error, "Argumentum: a bemenet �s a kimenet f�jl neve.");
  end if;
end Rendezes;

