package body Taszkseged is

  task body Szemafor is
     Bent: Natural := 0;
  begin
     loop
        select
           when Bent < Max => accept P; Bent := Bent + 1;
	or
	   accept V; Bent := Bent - 1;
	or
	   terminate;
	end select;
     end loop;
  end Szemafor;

  protected body Veletlen is
     procedure Reset is begin Ada.Numerics.Float_Random.Reset(G); Inicializalt := True; end Reset;
     entry General( F: out Float ) when Inicializalt is begin F := Ada.Numerics.Float_Random.Random(G); end General;
  end Veletlen;
  
begin
  Veletlen.Reset;
end Taszkseged;

