program factorial;
int Value, Count, Result;
begin
   read Value;
   Count := 1;
   Result := 1;
   while Count<Value do begin
      Count:=Count+1;
      Result:=Result*Count;
   end;;
   write Result;
end
