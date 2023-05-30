package body Matrixok is
   function "+" ( A, B : Matrix ) return Matrix is
      C : Matrix ( A'Range ( 1 ), A'Range ( 2 ));
   begin
      for I in C'Range ( 1 ) loop
         for J in C'Range ( 2 ) loop
            C ( I, J ) := A ( I, J ) + B ( I - A'First ( 1 ) + B'First ( 1 ),
                                           J - A'First ( 2 ) + B'First ( 2 ));
         end loop;
      end loop;
      return C;
   end "+";

   function "*" ( A, B : Matrix ) return Matrix is
      C : Matrix ( A'Range ( 1 ), B'Range ( 2 )) :=
             ( others => ( others => 0.0 ));
   begin
      for I in C'Range ( 1 ) loop
         for J in C'Range ( 2 ) loop
            for K in A'Range ( 2 ) loop
               C ( I, J ) := C ( I, J ) + A ( I, K ) * B ( K - A'First ( 2 ) +
                                                           B'First ( 1 ), J );
            end loop;
         end loop;
      end loop;
      return C;
   end "*";
end Matrixok;