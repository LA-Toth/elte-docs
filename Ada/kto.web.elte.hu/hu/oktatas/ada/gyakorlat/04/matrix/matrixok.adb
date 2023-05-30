package body Matrixok is
	
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
                                      J-A'First(2)+B'First(2) );
            end loop;
        end loop;
        return C;
    end "+";

    function "*" ( A, B: M�trix ) return M�trix is
        C: M�trix( A'Range(1), B'Range(2) ) := (others => (others => 0));
    begin
        for I in C'Range(1) loop
            for J in C'Range(2) loop
                for K in A'Range(2) loop
                    C(I,J) := C(I,J) + A(I,K-B'First(1)+A'First(2)) * 
                                       B(K-A'First(2)+B'First(1),J);
                end loop;
            end loop;
        end loop;
        return C;
    end "*";
    
    function "*" ( E: Elem; M: M�trix ) return M�trix is
        C: M�trix(M'Range(1),M'Range(2));
    begin
        for I in M'Range(1) loop
            for J in M'Range(2) loop
                C(I,J) := E * M(I,J);
            end loop;
        end loop;
        return C;
    end "*";

    procedure Beszoroz ( E: in Elem; M: in out M�trix ) is
    begin
        for I in M'Range(1) loop
            for J in M'Range(2) loop
                M(I,J) := E * M(I,J);
            end loop;
        end loop;
    end Beszoroz;

end Matrixok;

