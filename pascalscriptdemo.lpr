program pascalscriptdemo;

{$mode objfpc}{$H+}

(*
PascalScript demo application for Ultibo
Richard Metcalfe, November 2020

This application is a basic console application which demonstrates script execution
of PascalScript files. The code supports execution of a string based value
but this particular demo always loads code from scripts on the sd card.

Units;
script.pas - contains a basic object for script execution.
ScriptShell.pas - contains a shell command to execute a script from a source file
logoutput - basic logging support both on the console and on the telnet terminal

The application demonstrates the following features of PascalScript;
1.Compile and execute a PascalScript program, i.e. one that has
program <name> at the top, any procedures, and a begin...end. section.

2.Compile and execute an individual function defined in a script without any
program being defined, and to receive the function result.

3.Access application variables from the script, both to read their values
and to update them, and access the updated variables in the application thereafter

6.Make calls from the script to functions which are compiled into the Ultibo
binary, thereby effectively extending PascalScript.

*)


uses
  {$ifdef QEMU}
  QEMUVersatilePB,
  {$endif}
  {$ifdef ZERO}
  RaspberryPi,
  {$endif}
  {$ifdef RPI1}
  RaspberryPi,
  {$endif}
  {$ifdef RPI2}
  RaspberryPi2,
  {$endif}
  {$ifdef RPI3}
  RaspberryPi3,
  {$endif}
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  shell,
  RemoteShell,
  ShellFilesystem,
  ShellUpdate,
  script,
  scriptshell,
  framebuffer,
  console,
  logoutput,
  ConsoleShell
  ;

const QEMU = {$ifdef QEMU} True {$else} False {$endif};
var
  InfoWindowHandle : TWindowHandle;

procedure DisplayValues;
var
  ypos : integer;
begin
  ypos := 1;
  ConsoleWindowWriteEx(InfoWindowHandle, 'rpm           : ' + inttostr(rpm) + '    ',
     1, ypos + 1, COLOR_BLACK, COLOR_WHITE);
  ConsoleWindowWriteEx(InfoWindowHandle, 'TPS           : ' + inttostr(tps) + '    ',
     1, ypos + 2, COLOR_BLACK, COLOR_WHITE);
  ConsoleWindowWriteEx(InfoWindowHandle, 'MAP           : ' + inttostr(map) + '    ',
     1, ypos + 3, COLOR_BLACK, COLOR_WHITE);
  ConsoleWindowWriteEx(InfoWindowHandle, 'FloatValue    : ' + floattostr(floatvalue) + '    ',
     1, ypos + 4, COLOR_BLACK, COLOR_WHITE);
end;

begin
  LogWindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);

  InfoWindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

  Log('PascalScript for Ultibo Demo Application');
  Log('R. Metcalfe, November 2020');
  Log('');

  if QEMU then
  begin
    Log('Running under QEMU - See QEMULauncher.ini.');
    Log('Standard telnet port 23 is accessed through local port 7023.');
    Log('Therefore try "telnet localhost 7023"')
  end else begin
    Log('To use this application, open a telnet terminal using putty or a similar');
    Log('application and connect to your raspberry Pi on the standard telnet port.');
    Log('Your Pi must be connected to a wired network.')
  end;

  // assign some values to the globals the script will access
  // see the script unit for declaration of these variables and how they are
  // passed in and out of the scripts.

  rpm := 1000;
  map := 45;
  floatvalue := 3.1415927;
  tps := 50;

  // lets loop forever displaying some data on the screen
  // these data items are those that can be accessed and changed by the
  // scripts. They are exposed to the script engine via the script handler object.
  // refer to scriptshell.pas for the method of constructing these.

  while (true) do
  begin
     DisplayValues;
     sleep(100);
  end;
end.

