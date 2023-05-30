with Ada.Text_IO;
use Ada.Text_IO;

procedure Matrixos is
	
    type Index is new Integer;
    type Elem is new Integer;
    type M�trix is array (Index range <>, Index range <>) of Elem;

    procedure Hozz�ad( A: in out M�trix; B: in M�trix ) is
    begin
        -- A := A+B;
        for I in A'Range(1) loop
            for J in A'Range(2) loop
                A(I,J) := A(I,J) + B( I-A'First(1)+B'First(1),
                                      J-A'First(2)+B'First(2) );
            end loop;
        end loop;
    end Hozz�ad;

    function "+"( A, B: M�trix ) return M�trix is
        -- C: M�trix := A;
        C: M�trix(A'Range(1),A'Range(2));
    begin
        -- Hozz�ad(C,B);
        -- return C;
        for I in A'Range(1) loop
            for J in A'Range(2) loop
                C(I,J) := A(I,J) + B( I-A'First(1)+B'First(1),
                                      J-A'First(2)-B'First(2) );
            end loop;
        end loop;
        return C;
    end "+";

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

    M: M�trix := ((1,2),(1,2),(3,2));

begin
    Hozz�ad(M,M);
    M := M + M;
    Ki�r(M);
end Matrixos;

