with Ada.Text_IO;

package body Eszkozok is

	protected body Veletlen is
		procedure Inicializal is
		begin
			Reset(G);
			Inicializalt := True;
		end Inicializal;

		entry Ido(
			Ertek: out Duration;
			Mettol: in Duration := 0.0;
			Meddig: in Duration := 1.0
			)
		when Inicializalt is
		begin
			Ertek := Duration( Random(G) ) * (Meddig-Mettol) + Mettol;
		end Ido;
	end Veletlen;

	task body Kiiro is
	begin
		loop
			select
				accept Kiir( Mit: in String ) do
					Ada.Text_IO.Put_Line(Mit);
				end Kiir;
			or
				terminate;
			end select;
		end loop;
	end Kiiro;

	protected body Szemafor is
		entry P when Szamlalo > 0 is
		begin
			Szamlalo := Szamlalo - 1;
		end P;

		procedure V is begin Szamlalo := Szamlalo + 1; end V;
	end Szemafor;

end Eszkozok;
