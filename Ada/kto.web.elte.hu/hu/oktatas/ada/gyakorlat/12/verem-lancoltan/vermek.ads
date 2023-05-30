with Ada.Finalization;
use Ada.Finalization;

generic

   type Elem is private;

package Vermek is

   type Verem is new Controlled with private;

   procedure Push( V: in out Verem; E: in Elem );
   procedure Pop( V: in out Verem; E: out Elem ); -- Üres_Verem, ha üres
   procedure Pop( V: in out Verem );              -- Üres_Verem, ha üres
   function Top( V: Verem ) return Elem;          -- Üres_Verem, ha üres
   function Is_Empty( V: Verem ) return Boolean;
   function Size( V: Verem ) return Natural;

   Ures_Verem: exception;

   generic
      with procedure Action( E: in Elem );
   procedure Apply_On_All_Elements( V: in Verem );

   function "="( V1, V2: Verem ) return Boolean;
   procedure Adjust( V: in out Verem );
   procedure Finalize( V: in out Verem );

private

   type Csucs;
   type Mutato is access Csucs;
   type Csucs is record
                    Adat: Elem;
		    Kovetkezo: Mutato;
                 end record;

   type Verem is new Controlled with record
                    Veremteto: Mutato := null; 
		    Meret: Natural := 0;
                 end record;

end Vermek;
