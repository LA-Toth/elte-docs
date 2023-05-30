program greatest_common_divisor;
int x, y;
begin
        read x;
        read y;
        if 0=(x*x)+(y*y) then write 0-1
        else begin
                if x<0 then x:=0-x else
		   if y>=0 then else y:=0-y;
                while x*y\=0 do if x>y then x:=x-y else y:=y-x;
                write x+y;
        end
end
