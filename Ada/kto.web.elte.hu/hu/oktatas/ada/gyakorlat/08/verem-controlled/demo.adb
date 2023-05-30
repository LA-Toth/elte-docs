with Int_Vermek, Ada.Text_IO, Ada.Integer_Text_IO;
use Int_Vermek, Ada.Text_IO, Ada.Integer_Text_IO;

procedure Demo is

    V, W: Verem;
    N: Integer;

begin

    Push(V,1); Push(V,2);
    Push(W,3); Push(W,4);
    Push(V,5); Push(V,6);
    W := V;
    if V=W then Put_Line("ok"); end if;
    Pop(W,N); Put(N);
    if V/=W then Put_Line("ok"); end if;
    Pop(W,N); Put(N);
    Pop(W,N); Put(N);
    Pop(W,N); Put(N);
    Pop(V,N); Put(N);
    Pop(V,N); Put(N);
    Pop(V,N); Put(N);
    Pop(V,N); Put(N);

end Demo;

