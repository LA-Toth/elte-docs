with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;
package Eszkozok is

	protected Veletlen is
		procedure Inicializal;
		entry Ido(
			Ertek: out Duration;
			Mettol: in Duration := 0.0;
			Meddig: in Duration := 1.0
			);
	private
		G: Generator;
		Inicializalt: Boolean := False;
	end Veletlen;

	task Kiiro is
		entry Kiir( Mit: in String );
	end Kiiro;

	protected type Szemafor( Max: Positive := 1 ) is
		entry P;
		procedure V;
	private
		Szamlalo: Natural := Max;
	end Szemafor;

end Eszkozok;
