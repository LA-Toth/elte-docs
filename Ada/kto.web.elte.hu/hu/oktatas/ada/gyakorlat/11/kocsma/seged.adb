with Ada.Text_IO, Ada.Numerics.Float_Random;
use Ada.Text_IO, Ada.Numerics.Float_Random;

package body Seged is

  task Kiíró is
    entry Kiír( S: in String );
  end Kiíró;

  task body Kiíró is
  begin
     loop
         select
            accept Kiír( S: in String ) do Ada.Text_IO.Put_Line(S); end;
         or terminate;
         end select;
     end loop;
  end Kiíró;
	
  procedure Put_Line( S: in String ) is begin Kiíró.Kiír(S); end;

  task Véletlen_Gyártó is
     entry Véletlen ( F: out Float );
  end Véletlen_Gyártó;

  task body Véletlen_Gyártó is
     G: Generator;
  begin
     Reset(G);
     loop
         select
            accept Véletlen( F: out Float ) do F := Random(G); end;
         or terminate;
         end select;
     end loop;
  end Véletlen_Gyártó;

  function Véletlen return Float is
     F: Float;
  begin
     Véletlen_Gyártó.Véletlen(F);
     return F;
  end Véletlen;

  task body Szemafor is
     Bent: Natural := 0;
  begin
     loop
        select
           when Bent < Max => accept P; Bent := Bent + 1;
        or accept V; Bent := Bent - 1;
        or terminate;
        end select;
     end loop;
  end Szemafor;

end Seged;

