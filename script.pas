unit script;

{$mode objfpc}{$H+}

interface

uses
  uPSCompiler,
  uPSComponent,
  uPSRuntime,
  uPSUtils,
  classes,
  SysUtils;

type
  TPascalScriptHandler = class
  private
    FScript : TPSScript;
    FCompiled : Boolean;
    FScriptLines : TStringList;
    procedure scriptoncompile(sender : tpsscript);
    procedure scriptonexecute(sender : tpsscript);
    procedure afterexec(sender : tpsscript);
    procedure CompileLoadedScript;
  protected
    procedure MyLocalMethod(i : integer);
  public
    constructor Create(aScriptString : string);
    constructor CreateFromFile(afilename : string);
    destructor Destroy; override;
    function ExecuteStringFunction(fname : string) : string;     // kinda deprecated in favour of variant version but still works
    function ExecuteBooleanFunction(fname : string) : boolean;   // kinda deprecated in favour of variant version but still works
    function ExecuteIntegerFunction(fname : string) : Integer;   // kinda deprecated in favour of variant version but still works
    function ExecuteVariantFunction(fname : string) : Variant;
    procedure Execute;
    property Compiled : Boolean read FCompiled;
  end;


var
  rpm : word;
  map : word;
  tps : byte;
  floatvalue : extended;

implementation

uses
  logoutput;

procedure printfunc(aval : integer);
begin
  log(inttostr(aval));
end;

constructor TPascalScriptHandler.Create(aScriptString : string);
begin
  // string version of constructor. Script is contained entirely within the
  // constructor parameter.
  inherited Create;

  FScriptLines := tstringlist.create;
  FScriptLines.Add(aScriptString);
  FScript :=  TPSScript.Create(nil);
  FScript.Script := FScriptLines;

  CompileLoadedScript;
end;

constructor TPascalScriptHandler.CreateFromFile(afilename : string);
begin
  // file version of constructor. Script is loaded from a file given by the
  // filename. Filename must contain the entire path.
  inherited Create;

  FScriptLines := tstringlist.create;
  FScriptLines.LoadFromFile(afilename);
  FScript :=  TPSScript.Create(nil);
  FScript.Script := FScriptLines;

  CompileLoadedScript;
end;

destructor TPascalScriptHandler.Destroy;
begin
  FScript.Free;
  FScriptLines.Free;

  inherited Destroy;
end;

procedure TPascalScriptHandler.MyLocalMethod(i : integer);
begin
  log('MyLocalMethod called with ' + inttostr(i) + ' as a parameter');
end;

procedure TPascalScriptHandler.CompileLoadedScript;
var
  i : integer;
begin
  // setup event handlers to enable transfer of globals into and out of script
  // and definition of language extensions.
  FScript.OnCompile:= @scriptoncompile;
  FScript.OnExecute:= @scriptonexecute;
  FScript.OnAfterExecute := @afterexec;

  // compile the script. Report any failures to the log file.
  FCompiled := True;
  if not FScript.Compile then
  begin
    log('Failed to compile script: ' + inttostr(FScript.CompilerMessageCount) + ' errors');
    for i := 0 to FScript.compilermessagecount - 1 do
      log(FScript.CompilerMessages[i].MessageToString);
    log(FScriptLines[0]);
    FCompiled := false;
  end
  else
    log('successfully compiled script ' + leftstr(FScriptLines[0], pos(';', FScriptLines[0])));
end;

procedure TPascalScriptHandler.scriptoncompile(sender : tpsscript);
begin
  // the oncompile notification is a cue to add in variables and other definitions
  // that you want the script to be able to access.

  // variables (global in our app in this example)
  sender.AddRegisteredVariable('rpm', 'word');
  sender.AddRegisteredVariable('map', 'word');
  sender.AddRegisteredVariable('tps', 'byte');
  sender.AddRegisteredVariable('floatvalue', 'extended');

   // these two global procedures can be called from the script
  Sender.AddFunction(@printfunc, 'procedure print(aval : integer)');
  sender.AddFunction(@Log,'procedure Log(str: string)');
end;

procedure TPascalScriptHandler.afterexec(sender : tpsscript);
var
  i : integer;
begin
  // after execution.
  // grab any updated values from the script and update the globals with them.

  rpm := VGetInt(Sender.GetVariable('rpm'));
  map := VGetInt(Sender.GetVariable('map'));
  tps := VGetInt(Sender.GetVariable('tps'));
  floatvalue := VGetReal(Sender.GetVariable('floatvalue'));
end;

procedure TPascalScriptHandler.scriptonexecute(sender : tpsscript);
var
  i : integer;
  v : PIFVariant;
begin
  // just as execution begins, grab the globals and tell the script engine what
  // their current values are.
  VSetInt(Sender.GetVariable('rpm'), rpm);
  VSetInt(Sender.GetVariable('map'), map);
  VSetInt(Sender.GetVariable('tps'), tps);
  VSetReal(Sender.GetVariable('floatvalue'), floatvalue);
end;

function TPascalScriptHandler.ExecuteStringFunction(fname : string) : string;
begin
  // call a function which returns a string. No support for parameters here but is is possible to add them.
  if (FScript <> nil) then
    Result := FScript.ExecuteFunction([], fname)
  else
    log('There is no script object to execute');
end;

function TPascalScriptHandler.ExecuteBooleanFunction(fname : string) : boolean;
begin
  // call a function which returns a boolean. No support for parameters here but is is possible to add them.
  if (FScript <> nil) then
    Result := FScript.ExecuteFunction([], fname)
  else
    log('There is no script object to execute');
end;

function TPascalScriptHandler.ExecuteIntegerFunction(fname : string) : Integer;
begin
  // call a function which returns an integer. No support for parameters here but is is possible to add them.
  Result := 0;
  if (FScript <> nil) then
    Result := FScript.ExecuteFunction([], fname)
  else
    log('There is no script object to execute');
end;

function TPascalScriptHandler.ExecuteVariantFunction(fname : string) : Variant;
begin
  // call a function which returns any type. No support for parameters here but is is possible to add them.
  // this method replaces all of the others, although it is slightly less efficient due to
  // the way variant types work.

  Result := 0;
  if (FScript <> nil) then
    Result := FScript.ExecuteFunction([], fname)
  else
    log('There is no script object to execute');
end;


procedure TPascalScriptHandler.Execute;
begin
   if (FScript <> nil) then
     if (not FScript.Execute) then
       log('Execution of script failed');
end;

end.
