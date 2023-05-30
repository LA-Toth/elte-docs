with Ada.Text_IO, Rendez;
use Ada.Text_IO;

procedure Rendezes is    
	
    type Tömb is array (Character range <>) of Float;
    procedure Rendez_Növ is new Rendez(Float,Character,Tömb);
    procedure Rendez_Csökk is new Rendez(Float,Character,Tömb,">");

    T: Tömb := (3.0,6.2,1.7,5.2,3.9);

begin
    Rendez_Csökk(T);
    for I in T'Range loop
       Put_Line( Float'Image( T(I) ) );
    end loop;
end Rendezes;
