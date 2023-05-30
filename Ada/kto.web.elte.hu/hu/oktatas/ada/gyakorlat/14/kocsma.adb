with Eszkozok; use Eszkozok;
procedure Kocsma is

	Ajto: Szemafor(4);

	type Ital is (Sor, Bor, Palinka);

	task Kocsmaros is
		entry Tolt( Mit: in Ital );
	end Kocsmaros;

	task body Kocsmaros is
		Dolgozik: Boolean := True;
		Toltes_Ido: constant array(Ital) of Duration := (1.0, 0.5, 0.7);
	begin
		while Dolgozik loop
			select
				accept Tolt( Mit: in Ital ) do
					Kiiro.Kiir( Ital'Image(Mit) & "t toltok" );
					delay Toltes_Ido(Mit);
				end Tolt;
			or
				delay 2.0;
				Dolgozik := False;
			end select;
		end loop;
		Kiiro.Kiir("Kocsmaros hazamegy.");
	end Kocsmaros;

	task type Reszeg;
	task body Reszeg is

		procedure Kocsmaba_Megy is
			Sikerult: Boolean := False;
		begin
			while not Sikerult loop
				select
					Ajto.P;
					Sikerult := True;
				else
					Kiiro.Kiir("Elmegyek szunditani egyet.");
					delay 2.0;
				end select;
			end loop;
		end Kocsmaba_Megy;

		procedure Italozik is
			Sorivas_Ideje: Duration := 1.0;
		begin
			Kocsmaros.Tolt(Palinka); Kiiro.Kiir("Palinkat iszom..."); delay 0.1;
			Kocsmaros.Tolt(Bor); Kiiro.Kiir("Bort iszom..."); delay 0.5;
			loop
				Kocsmaros.Tolt(Sor); Kiiro.Kiir("Sort iszom..."); 
				delay Sorivas_Ideje;
				Sorivas_Ideje := 2 * Sorivas_Ideje;
			end loop;
		exception
			when Tasking_Error => Kiiro.Kiir("Nem kapok inni, hat elmegyek.");
			when others => Kiiro.Kiir("Nem tudom, mi tortent velem...");
		end Italozik;

	begin
		Kocsmaba_Megy;
		Italozik;
		Ajto.V;
	end Reszeg;

	Reszegek: array(1..2) of Reszeg;

	type String_Access is access String;

	task type Egyetemista( Nev: String_Access );
	task body Egyetemista is
		Veletlen_Ido: Duration;
	begin
		select
			Ajto.P;
			declare begin
				Kocsmaros.Tolt(Bor);
				Kiiro.Kiir( Nev.all & " bort iszik" );
				Veletlen.Ido(Veletlen_Ido,0.5,1.5);
				delay Veletlen_Ido;
			exception
				when Tasking_Error => 
					Kiiro.Kiir(Nev.all & " nem kap inni, elmegy eloadasra.");
				when others => 
					Kiiro.Kiir(Nev.all & " nem tudja, mi tortent vele...");
			end;
			Ajto.V;
		or
			delay 1.0;
			Kiiro.Kiir(Nev.all & " ivas helyett tanulasra adja a fejet.");
		end select;
	end;

	type Egyetemista_Access is access Egyetemista;
	EP: Egyetemista_Access;

begin
	Veletlen.Inicializal;
	delay 0.2; EP := new Egyetemista( new String'("Jani") );
	delay 0.2; EP := new Egyetemista( new String'("Peti") );
	delay 0.2; EP := new Egyetemista( new String'("Kati") );
end Kocsma;
