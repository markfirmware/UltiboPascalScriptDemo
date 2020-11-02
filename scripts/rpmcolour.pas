function rpmcolour : string;
begin
  if (rpm < 3000) then
    Result := 'Green'
  else
  if (rpm < 5000) then
   Result := 'Amber'
  else
  if (rpm < 7000) then
    Result := 'Slightly pink'
  else
    Result := 'RED!!!!!';
end;

begin
end.
