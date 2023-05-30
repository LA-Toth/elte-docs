with Ada.Text_IO;
use Ada.Text_IO;

procedure Matrixos is
	
    type Index is new Integer;
    type Elem is new Integer;
    type Mátrix is array (Index range <>, Index range <>) of Elem;

    procedure Hozzáad( A: in out Mátrix; B: in Mátrix ) is
    begin
        -- A := A+B;
        for I in A'Range(1) loop
            for J in A'Range(2) loop
                A(I,J) := A(I,J) + B( I-A'First(1)+B'First(1),
                                      J-A'First(2)+B'First(2) );
            end loop;
        end loop;
    end Hozzáad;

    function "+"( A, B: Mátrix ) return Mátrix is
        -- C: Mátrix := A;
        C: Mátrix(A'Range(1),A'Range(2));
    begin
        -- Hozzáad(C,B);
        -- return C;
        for I in A'Range(1) loop
            for J in A'Range(2) loop
                C(I,J) := A(I,J) + B( I-A'First(1)+B'First(1),
                                      J-A'First(2)+B'First(2) );
            end loop;
        end loop;
        return C;
    end "+";

    function "*" ( A, B: Mátrix ) return Mátrix is
        C: Mátrix( A'Range(1), B'Range(2) ) := (others => (others => 0));
    begin
        for I in C'Range(1) loop
            for J in C'Range(2) loop
                for K in A'Range(2) loop
                    C(I,J) := C(I,J) + A(I,K-B'First(1)+A'First(1)) * 
                                       B(K-A'First(2)+B'First(1),J);
                end loop;
            end loop;
        end loop;
        return C;
    end "*";
    
    function "*" ( E: Elem; M: Mátrix ) return Mátrix is
        C: Mátrix(M'Range(1),M'Range(2));
    begin
        for I in M'Range(1) loop
            for J in M'Range(2) loop
                C(I,J) := E * M(I,J);
            end loop;
        end loop;
        return C;
    end "*";

    procedure Beszoroz ( E: in Elem; M: in out Mátrix ) is
    begin
        for I in M'Range(1) loop
            for J in M'Range(2) loop
                M(I,J) := E * M(I,J);
            end loop;
        end loop;
    end Beszoroz;

    procedure Kiír ( M: in Mátrix ) is
    begin
        for I in M'Range(1) loop
            for J in M'Range(2) loop
                Put( Elem'Image(M(I,J)) );
                Put( Ascii.HT );
            end loop;
            New_Line;
        end loop;
    end Kiír;

    M: Mátrix := ((1,2),(1,2),(3,2));
    I: Mátrix := ((1,0),(0,1));

begin
    M := 2 * M * I;
    Beszoroz(2,M);
    Kiír(M);
end Matrixos;

