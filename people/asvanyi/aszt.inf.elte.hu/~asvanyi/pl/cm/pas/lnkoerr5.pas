program lnko;
int x, y;
begin
   read x;
   read y;
   while x\=y do
   begin
      if x>y then
	 x:=x-y
      else
	 y:=y--x
   end;
   write x
end.

This is comment.
