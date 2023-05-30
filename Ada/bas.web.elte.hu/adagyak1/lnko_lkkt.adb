with Text_IO, Kozos;
use Text_IO;

procedure LNKO_LKKT is
   P, Q : Positive;  
begin
   Kozos ( 12, 18, P, Q );
   Put_Line ( "(12;18)=" & Positive'Image ( P ));
   Put_Line ( "[12;18]=" & Positive'Image ( Q ));
end LNKO_LKKT;