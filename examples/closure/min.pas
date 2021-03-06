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

function counter(L: Plua_state): Integer; cdecl;
var
  val: double;
begin
  writeln('counter called');
  val := lua_tonumber(L, lua_upvalueindex(1)); //get the current value assigned to the current counter
  lua_pushnumber(L, val+1);  // new value
  lua_pushvalue(L, -1);  // duplicate it
  lua_replace(L, lua_upvalueindex(1));  // update upvalue
  Result := 1; //not needed?
end;

function lua_counter_create(L: Plua_state): Integer; cdecl;
begin
  writeln('create counter');
  lua_pushnumber(L, 0); //push 0 onto the stack
  lua_pushcclosure(L, counter, 1); //assing c function counter with 1 variable containing 0 from stack
  Result:= 1;
end;

//TODO: do something more fancy then a counter (e.g. simulate a more complete class)

procedure registerwithlua(L: Plua_State);
begin
  //register counter closure
  writeln('register counter');
  lua_pushcfunction(L,lua_counter_create);
  lua_setglobal(l, 'newCounter'); //so we can now call c1 = newCounter() in lua
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
    //lua_error(L);
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
