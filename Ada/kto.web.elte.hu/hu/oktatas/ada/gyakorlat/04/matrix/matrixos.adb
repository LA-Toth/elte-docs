with Ada.Text_IO, Matrixok;
use Ada.Text_IO, Matrixok;

procedure Matrixos is
	
    M: M�trix := ((1,2),(1,2),(3,2));
    I: M�trix := ((1,0),(0,1));

    procedure Ki�r ( M: in M�trix ) is
    begin
        for I in M'Range(1) loop
            for J in M'Range(2) loop
                Put( Elem'Image(M(I,J)) );
                Put( Ascii.HT );
            end loop;
            New_Line;
        end loop;
    end Ki�r;

begin
	
    M := 2 * M * I;
    Beszoroz(2,M);
    Ki�r(M);

end Matrixos;

