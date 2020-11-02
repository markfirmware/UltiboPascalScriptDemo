unit logoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Console, GlobalTypes, MonitorShell;

var
  LogWindowHandle : TWindowHandle = 0;

procedure Log(str : string);

implementation

procedure Log(str : string);
var
  s : string;
begin
  s := DateTimeToStr(Now) +': ' + str;

  if (LogWindowHandle <> 0) then
    ConsoleWindowWriteLn(LogWindowHandle, s);

  moncmd.log(s);
end;


end.

