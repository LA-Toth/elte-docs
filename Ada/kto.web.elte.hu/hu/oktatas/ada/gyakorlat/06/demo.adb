with Vermek;
procedure Demo is
    package Int_Vermek is new Vermek(Integer); 
    use Int_Vermek;
    V: Verem;
    X: Integer;
begin
    for I in 1..1000000 loop
       Push(V,10);
       Pop(V,X);
    end loop;
end;
