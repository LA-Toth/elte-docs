with Ada.Unchecked_Deallocation;
package body Sorok is

	procedure Betesz ( S: in out Sor; E: in Elem ) is
		�j: Mutat� := new Cs�cs ' (E,null);
	begin
		if S.V�ge = null then
			S.Eleje := �j;
		else
			S.V�ge.K�vetkez� := �j;
		end if;
		S.V�ge := �j;
	end Betesz;

	procedure Felszabad�t is
		     new Ada.Unchecked_Deallocation(Cs�cs, Mutat�);

	procedure Kivesz ( S: in out Sor; E: out Elem ) is
		R�gi: Mutat� := S.Eleje;
	begin
		if R�gi = null then raise �res_Sor;
		else	E := R�gi.Adat;
			if  S.Eleje = S.V�ge then  S.V�ge := null; end if;
			S.Eleje := S.Eleje.K�vetkez�;
			Felszabad�t(R�gi);
		end if;
	end Kivesz;

	procedure Finalize ( S: in out Sor ) is
		P: Mutat�;
	begin
		while S.Eleje /= null loop
			P := S.Eleje;
			S.Eleje := S.Eleje.K�vetkez�;
			Felszabad�t(P);
		end loop;
	end Finalize;

	procedure Iter�l ( S: in Sor ) is
		P: Mutat� := S.Eleje;
	begin
		while P /= null loop
			Feladat( P.Adat );
			P := P.K�vetkez�;
		end loop;
	end Iter�l;

end Sorok;

