with Ada.Finalization;
use Ada.Finalization;

generic

    type Index is (<>);          -- with positions 0,1,2...
    type Element is private;

package Vectors is

    type Vector is new Controlled with private;

    Index_Out_Of_Bounds: exception;

    procedure Add( V: in out Vector; E: in Element );
    procedure Set( V: in out Vector; I: in Index; E: in Element );
    function  Get( V: Vector; I: Index ) return Element;
    function  Size( V: Vector ) return Natural;
    function  Capacity( V: Vector ) return Natural;
    procedure Remove( V: in out Vector; I: in Index );
    -- procedure Insert( V: in out Vector; I: in Index; E: in Element );
    procedure Trim( V: in out Vector );
    -- procedure Ensure_Capacity( V: in out Vector; New_Capacity: Positive );

    function "="( V1, V2: Vector ) return Boolean;
    procedure Adjust( V: in out Vector );
    procedure Finalize( V: in out Vector );

private

    type Element_Array is array ( Index range <> ) of Element;
    type Element_Array_Access is access Element_Array;
    type Vector is new Controlled with record
                      Data: Element_Array_Access := new Element_Array(Index'First..Index'First);
		      Size: Natural := 0;
		      -- invariant: Data.all'Length > 0
                   end record;

end Vectors;
