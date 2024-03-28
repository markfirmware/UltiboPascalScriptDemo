unit ScriptShell;

{$mode objfpc}{$H+}

interface

uses
  Ultibo,
  platform,
  Classes,
  SysUtils,
  Shell;

type
  TEditShellCommand = class(TShellCommand)
  public
   constructor Create;
  public
   function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
  end;

  TScriptShellCommand = class(TShellCommand)
  public
   constructor Create;
  public
   function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
   function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
   function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
  end;

  TFunctionShellCommand = class(TShellCommand)
  public
   constructor Create;
  public
   function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
   function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
   function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
  end;


var
   ScriptCmd : TScriptShellCommand;
   FunctionCmd : TFunctionShellCommand;
   EditCmd : TEditShellCommand;
   IsEditorStarted : Boolean;


implementation

uses
  script,
  Variants,
  TextEditor,
  GlobalTypes,
  Console;


constructor TEditShellCommand.Create;
begin
 inherited Create;
 Name:='EDIT';
 Flags:=0;
end;

function TEditShellCommand.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;
var
 ScriptHandler : TPascalScriptHandler;
begin
 try
 Result:=False;

 if AShell = nil then Exit;

 RunEditor;

 except
   on e : Exception do
     AShell.DoOutput(Asession, 'Something went wrong with the editor: ' + e.message);
 end;
end;


constructor TScriptShellCommand.Create;
begin
 // command to execute a pascal program in PascalScript
 // i.e. one that has Program name; at the top
 // a series of functions, procedures, consts, types, etc
 // and a begin..end section.

 inherited Create;

 Name:='SCRIPT';

 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

function TScriptShellCommand.DoHelp(AShell:TShell;ASession:TShellSession):Boolean;
begin
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'SCRIPT - execute a PascalScript script');
 AShell.DoOutput(ASession,'SCRIPT <filename> - execute the script given by the filename');

 Result := True;
end;

function TScriptShellCommand.DoInfo(AShell:TShell;ASession:TShellSession):Boolean;
begin
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'No info for SCRIPT command at present');
 Result := True;
end;

function TScriptShellCommand.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;
var
 ScriptFile : String;
 ScriptHandler : TPascalScriptHandler;
begin
 try
 Result:=False;

 if AShell = nil then Exit;

 // get params
 ScriptFile := upcase(AShell.ParameterIndex(0,AParameters));

 if Length(ScriptFile) > 0 then
 begin
   // compile the script and execute it.
   AShell.DoOutput(ASession,'Executing script ' + ScriptFile);

   ScriptHandler := TPascalScriptHandler.CreateFromFile('c:\'+ScriptFile);
   if (ScriptHandler <> nil) and (ScriptHandler.Compiled) then
     ScriptHandler.Execute
   else
     AShell.DoOutput(ASession, 'Unabled to execute due to compile failure. Type "mon 1" to see the errors.');
 end;

 except
   on e : Exception do
     AShell.DoOutput(Asession, 'Something went wrong executing the script: ' + e.message);
 end;
end;


constructor TFunctionShellCommand.Create;
begin
 // a command to execute an individual function in a pascalscript source file
 // the whole file will be compiled, but it's more appropriate to put just the
 // function you want to call (and any other stuff that function calls
 // in a file by itself. The file must end with "begin end." otherwise the
 // compilation will fail.

 inherited Create;

 Name:='FUNCTION';

 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

function TFunctionShellCommand.DoHelp(AShell:TShell;ASession:TShellSession):Boolean;
begin
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'FUNCTION - execute a PascalScript function');
 AShell.DoOutput(ASession,'FUNCTION <function> <filename> - execute the named function in the file given by the filename');

 Result := True;
end;

function TFunctionShellCommand.DoInfo(AShell:TShell;ASession:TShellSession):Boolean;
begin
 Result:=False;

 if AShell = nil then Exit;

 AShell.DoOutput(ASession,'No info for FUNCTION command at present');
 Result := True;
end;

function TFunctionShellCommand.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;
var
 ScriptFile : String;
 FunctionName : string;
 ScriptHandler : TPascalScriptHandler;
 VResult : Variant;
begin
 try
 Result:=False;

 if AShell = nil then Exit;

 // load parameters
 FunctionName := upcase(AShell.ParameterIndex(0,AParameters));
 ScriptFile := upcase(AShell.ParameterIndex(1,AParameters));

 if (ScriptFile <> '') and (FunctionName <> '') then
 begin
   AShell.DoOutput(ASession,'Executing function ' + FunctionName + ' in ' + ScriptFile);

   // compile the script
   ScriptHandler := TPascalScriptHandler.CreateFromFile('c:\'+ScriptFile);
   if (ScriptHandler <> nil) and (ScriptHandler.Compiled) then
   begin
     // call the function and grab the result
     VResult := ScriptHandler.ExecuteVariantFunction(FunctionName);

     // result is a variant type, so format it accordingly and display on the
     // session output.
     // some variant types have been deliberately missed out of this list.
     AShell.DoOutputEx(ASession, 'Result: ', False);
     Case varType(VResult) of
        varEmpty:
            AShell.DoOutput(ASession,'Empty Result');
        varNull:
            AShell.DoOutput(ASession,'Null Result');
        varSingle:
            AShell.DoOutput(ASession, FloatToStr(VResult));
        varDouble:
            AShell.DoOutput(ASession,FloatToStr(VResult));
        varString:
            AShell.DoOutput(ASession,VResult);
        varBoolean:
            AShell.DoOutput(ASession,BoolToStr(VResult, True));
        varSmallint,
        varInteger,
        varInt64,
        varByte,
        varWord,
        varQWord,
        varLongWord,
        varShortInt:
            AShell.DoOutput(ASession,IntToStr(VResult));
     end;
   end
   else
     AShell.DoOutput(ASession, 'Unabled to execute due to compile failure. Type "mon 1" to see the errors.');
 end;

 except
   on e : Exception do
     AShell.DoOutput(Asession, 'Something went wrong executing the script: ' + e.message);
 end;
end;

initialization
  IsEditorStarted:=False;

  // create and register shell commands.
  ScriptCmd := TScriptShellCommand.Create;
  ShellRegisterCommand(ScriptCmd);

  EditCmd := TEditShellCommand.Create;
  ShellRegisterCommand(EditCmd);

  FunctionCmd := TFunctionShellCommand.Create;
  ShellRegisterCommand(FunctionCmd);

end.
