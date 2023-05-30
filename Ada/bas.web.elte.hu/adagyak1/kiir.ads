generic
   type T is limited private;

   with function To_String ( X : T ) return String;

procedure Kiir ( X : in T );