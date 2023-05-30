function Fakt_E ( Number : Natural ) return Natural is
begin
   return Number * Fakt_E ( Number - 1 );
exception
   when Constraint_Error =>
      return 1;
end Fakt_E;