with Ada.Unchecked_Deallocation;
package body Vectors is

    procedure Check_Index( V: in Vector; I: in Index ) is
    begin
        if Index'Pos(I) >= V.Size then
            raise Index_Out_Of_Bounds;
	end if;
    end Check_Index;

    procedure Add( V: in out Vector; E: in Element ) is
    begin
        --if Size(V) = Capacity(V) then
	--    Ensure_Capacity(V,2*Capacity(V));
	--end if;
	V.Data.all(Index'Val(V.Size)) := E;
        V.Size := V.Size + 1;
    end Add;
    
    procedure Set( V: in out Vector; I: in Index; E: in Element ) is
    begin
	Check_Index(V,I);
        V.Data.all(I) := E;
    end Set;

    function  Get( V: Vector; I: Index ) return Element is
    begin
	Check_Index(V,I);
        return V.Data.all(I);
    end Get;

    function  Size( V: Vector ) return Natural is
    begin
        return V.Size;
    end Size;

    function  Capacity( V: Vector ) return Natural is
    begin
        return V.Data.all'Length;
    end Capacity;

    procedure Remove( V: in out Vector; I: in Index ) is
    begin
	Check_Index(V,I);
        V.Data.all( I .. Index'Pred(Index'Val(V.Size-1)) ) := V.Data.all( Index'Succ(I) .. Index'Val(V.Size-1) );
	V.Size := V.Size - 1;
    end Remove;
    
    -- procedure Insert( V: in out Vector; I: in Index; E: in Element );
    
    procedure Free is new Ada.Unchecked_Deallocation(Element_Array,Element_Array_Access);
    
    procedure Trim( V: in out Vector ) is
        New_Capacity: Positive := Integer'Max(V.Size,1);
	New_Data: Element_Array_Access := new Element_Array(Index'First..Index'Val(New_Capacity-1));
    begin
	if V.Size /= 0 then
            New_Data.all := V.Data.all(Index'First..Index'Val(V.Size-1));
	end if;
	Free(V.Data);
	V.Data := New_Data;
    end Trim;
    
    -- procedure Ensure_Capacity( V: in out Vector; New_Capacity: Positive );

    function "="( V1, V2: Vector ) return Boolean is
    begin
        if V1.Size = V2.Size then
            for I in 0 .. V1.Size-1 loop
	        if V1.Data(Index'Val(I)) /= V2.Data(Index'Val(I)) then
		    return False;
		end if;
	    end loop;
	    return True;
        else
	    return False;
	end if;
    end "=";

    procedure Adjust( V: in out Vector ) is
    begin
        V.Data := new Element_Array'(V.Data.all);
    end Adjust;

    procedure Finalize( V: in out Vector ) is
    begin
        Free(V.Data);
    end Finalize;

end Vectors;
