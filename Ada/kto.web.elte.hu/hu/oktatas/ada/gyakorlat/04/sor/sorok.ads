package Sorok is

      subtype Elem is Integer;

      type Sor( Max: Positive ) is limited private;

      procedure Hiext( S: in out Sor; E: in Elem );
      procedure Lopop( S: in out Sor; E: out Elem );
      function Lov( S: Sor ) return Elem;
      function Is_Empty( S: Sor ) return Boolean;
      function Is_Full( S: Sor ) return Boolean;
      function Size( S: Sor ) return Natural;

private

      type Tömb is array ( Integer range <> ) of Elem;
      type Sor( Max: Positive ) is record
                                       Adatok: Tömb(1..Max);
                                       Putter, Getter: Positive := 1;
                                       Size: Natural := 0;
                                   end record;

end Sorok;

