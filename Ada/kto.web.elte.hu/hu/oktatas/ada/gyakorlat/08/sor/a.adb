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

	function �sszeg ( S: Sor ) return Integer is
		N: Integer := 0;
		procedure Hozz�ad ( E: in Integer ) is begin  N := N + E; end;
		procedure �sszegez is new Iter�l(Hozz�ad);
	begin
		�sszegez(S);
		return N;
	end �sszeg;

begin
	--for J in Natural loop Ada.Integer_Text_IO.Put(J); B; end loop;
	declare begin Kivesz(S,N); exception when �res_Sor => Put(1); end;
	Betesz(S,1); Kivesz(S, N); Put(N); Put(�sszeg(S));
	Betesz(S,2); Betesz(S,1); Kivesz(S, N); Put(N); Kivesz(S, N); Put(N);
	Betesz(S,3); Betesz(S,2); Betesz(S,1); Put(�sszeg(S));
	Kivesz(S, N); Put(N); Kivesz(S, N); Put(N); Kivesz(S, N); Put(N);
	Betesz(S,2); Betesz(S,1); Kivesz(S, N); Put(N);
	Betesz(S,2); Betesz(S,1); Kivesz(S, N); Put(N);
	Kivesz(S, N); Put(N); Kivesz(S, N); Put(N); Kivesz(S, N); Put(N);
end A;

