with Ada.Text_IO, Rendez;
use Ada.Text_IO;

procedure Rendezes is    
	
    type T�mb is array (Character range <>) of Float;
    procedure Rendez_N�v is new Rendez(Float,Character,T�mb);
    procedure Rendez_Cs�kk is new Rendez(Float,Character,T�mb,">");

    T: T�mb := (3.0,6.2,1.7,5.2,3.9);

begin
    Rendez_Cs�kk(T);
    for I in T'Range loop
       Put_Line( Float'Image( T(I) ) );
    end loop;
end Rendezes;
