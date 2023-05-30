with Int_Sorok, Ada.Integer_Text_IO;
use Int_Sorok, Ada.Integer_Text_IO;
procedure A is
	procedure B is
		S: Int_Sorok.Sor;
	begin
		Int_Sorok.Betesz(S,2);
	end B;
	S: Sor;
	N: Integer;

	function Összeg ( S: Sor ) return Integer is
		N: Integer := 0;
		procedure Hozzáad ( E: in Integer ) is begin  N := N + E; end;
		procedure Összegez is new Iterál(Hozzáad);
	begin
		Összegez(S);
		return N;
	end Összeg;

begin
	--for J in Natural loop Ada.Integer_Text_IO.Put(J); B; end loop;
	declare begin Kivesz(S,N); exception when Üres_Sor => Put(1); end;
	Betesz(S,1); Kivesz(S, N); Put(N); Put(Összeg(S));
	Betesz(S,2); Betesz(S,1); Kivesz(S, N); Put(N); Kivesz(S, N); Put(N);
	Betesz(S,3); Betesz(S,2); Betesz(S,1); Put(Összeg(S));
	Kivesz(S, N); Put(N); Kivesz(S, N); Put(N); Kivesz(S, N); Put(N);
	Betesz(S,2); Betesz(S,1); Kivesz(S, N); Put(N);
	Betesz(S,2); Betesz(S,1); Kivesz(S, N); Put(N);
	Kivesz(S, N); Put(N); Kivesz(S, N); Put(N); Kivesz(S, N); Put(N);
end A;

