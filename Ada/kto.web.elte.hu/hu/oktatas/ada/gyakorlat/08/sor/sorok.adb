with Ada.Unchecked_Deallocation;
package body Sorok is

	procedure Betesz ( S: in out Sor; E: in Elem ) is
		Új: Mutató := new Csúcs ' (E,null);
	begin
		if S.Vége = null then
			S.Eleje := Új;
		else
			S.Vége.Következõ := Új;
		end if;
		S.Vége := Új;
	end Betesz;

	procedure Felszabadít is
		     new Ada.Unchecked_Deallocation(Csúcs, Mutató);

	procedure Kivesz ( S: in out Sor; E: out Elem ) is
		Régi: Mutató := S.Eleje;
	begin
		if Régi = null then raise Üres_Sor;
		else	E := Régi.Adat;
			if  S.Eleje = S.Vége then  S.Vége := null; end if;
			S.Eleje := S.Eleje.Következõ;
			Felszabadít(Régi);
		end if;
	end Kivesz;

	procedure Finalize ( S: in out Sor ) is
		P: Mutató;
	begin
		while S.Eleje /= null loop
			P := S.Eleje;
			S.Eleje := S.Eleje.Következõ;
			Felszabadít(P);
		end loop;
	end Finalize;

	procedure Iterál ( S: in Sor ) is
		P: Mutató := S.Eleje;
	begin
		while P /= null loop
			Feladat( P.Adat );
			P := P.Következõ;
		end loop;
	end Iterál;

end Sorok;

