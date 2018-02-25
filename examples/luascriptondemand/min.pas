program min;

{$APPTYPE CONSOLE}

// defines to configure freepascal
{$IFDEF FPC}
  {$MODE Delphi}

  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

uses
  SysUtils,
  Classes,
  dlua;

//function to print lua data via delphi
function lua_print(L: Plua_State): Integer; cdecl;
var
  i, n: Integer;
begin
  write('Lua: ');
  n := lua_gettop(L);
  for i := 1 to n do
  begin
    if i > 1 then
      Write(#9);
    if lua_isstring(L, i) then
      Write(lua_tostring(L, i))
    else
      Write(Format('%s:%p', [lua_type(L, i), lua_topointer(L, i)]));
  end;
  WriteLn;
  Result := 0;
end;

function MyLoader(L: Plua_State):Integer; cdecl;
var
  name: string;
  script: TStringList;
  pscript: pchar;
  s: Integer;
begin
  writeln('myloader');
  //load the first parameter
  //which is the name of the file, or whatever string identifier for a resource that you passed in with require()
  name := lua_tostring(L,1);

  //generate our custom dynamic script
  script:=tstringList.Create;
  script.Add('--generated test script');
  script.Add('print("init myloader lua part")');
  script.Add('function foo()');
  script.Add('  print("hello from '+name+'")');
  script.Add('end');

  //load the buffer
  pscript:=script.gettext;
  s:=lual_loadbuffer(L, pscript, length(pscript), 'myluascript');
  Writeln(inttostr(s)); //debug if load buffer worked
  StrDispose(pscript);
  freeAndNil(script);
  //compile the lua script so that other scripts can see it
  s:=lua_pcall (L, 0, 0, 0); //add it to the scripts
  Writeln(inttostr(s)); //debug if pcall worked
  writeln('end myloader');

  Result := 1;

end;

procedure registerwithlua(L: Plua_State);
begin
  //register with lua
  luaL_openlibs(L); //make require work
  luaL_requiref( L, 'mymodule', MyLoader, 1 ); //register mymodule so it can be called with require "mymodule"
                                               //MyLoader provides the content of mymodule
  lua_pop(L, 1); //restore lua stack

end;

var
  L: Plua_State = nil; //lua state
  script: tstringlist; //a stringlist to hold the lua script
  result: integer;     //0 if script executes ok
  pscript: pchar;

begin

  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
     DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  if ParamCount <= 0 then
  begin
    WriteLn('Usage: min.exe filename');
    Exit;
  end;

  writeln('load lua');
  //init lua dll
  LoadLua;

  writeln('about to open state');
  L := lua_open;
  writeln('about to register');

  //register pseudo delphi array (class) in lua
  registerwithlua(L);

  writeln('register print');
  
  //Register a delphi procedure/funtion for use in Lua
  lua_register(L, 'print', lua_print);

  writeln('load lua script');
  //Load a lua script from a buffer
  script:=tstringList.Create;
  script.LoadFromFile(PChar(ParamStr(1)));
  pscript:=script.gettext;
  lual_loadbuffer(L, pscript, length(pscript), 'myluascript');
  StrDispose(pscript); //clean up
  freeAndNil(script); //clean up

  writeln('run lua script');
  
  //Ask Lua to run our little script
  result := 0;
  result := lua_pcall(l, 0, LUA_MULTRET, 0); //TODO: reimplment
  if result>0 then
  begin
    writeln('bad, bad script'); //should provide more usefull info
    lua_error(L);
    Writeln('press [ENTER] key to exit...');
    ReadLn;
    exit;                       //stop the program
  end;

  writeln('done with script');

  //close lua dll
  lua_close(L);
  UnLoadLua;

  Writeln('press [ENTER] key to exit...');
  ReadLn;

end.
