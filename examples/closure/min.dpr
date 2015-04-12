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
  writeln('n: '+inttostr(n));
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

//TODO: do something more fancy then a counter (e.g. simulate a more complete class)

function lua_counter_create(L: Plua_state): Integer; cdecl;
begin
  writeln('create counter');


  //lua_pushlightuserdata (L, this); //add the/an object to the stack
  lua_pushnumber(L, 0); //push 0 onto the stack
  lua_pushcclosure(L, counter, 1); //assing c function counter with 1 variable containing 0 from stack
  // so now we can call print(c1()) in lua what will call the counter function above here



  writeln('3');

  Result:= 1;
end;

function lua_myobject_create(L: Plua_state): Integer; cdecl;
begin
  writeln('create myobject counter');

  lua_newtable(L);

  writeln('0');
  //function test
  lua_pushnumber(L, 0);

  luaL_getmetatable(L, 'delphi.test');
  //lua_getfield(L, LUA_REGISTRYINDEX, 'delphi.test');
  writeln('1');
  lua_setmetatable(L, -2);
  writeln('2');
  lua_settable(L, -3);
  writeln('3');

  lua_pushstring(L, 'get');
  writeln('4');

  //lua_pushlightuserdata (L, this); //add the/an object to the stack
  lua_pushnumber(L, 0); //push 0 onto the stack
  lua_pushcclosure(L, counter, 1); //assing c function counter with 1 variable containing 0 from stack
  // so now we can call print(c1()) in lua what will call the counter function above here

  lua_settable(L, -3);

  //property test
  lua_pushliteral(L, 'j');
  lua_pushnumber(L, 5); //we set value 5 to property j
  lua_settable(L, -3);


  writeln('5');

  Result:= 1;
end;

const
  methodslib: array [0..1] of luaL_reg = (
   (name:'new';func:lua_myobject_create),
   (name:nil;func:nil)
   );


procedure registerwithlua(L: Plua_State);
begin
  writeln('register counter');
  lua_pushcfunction(L,lua_counter_create);
  lua_setglobal(l, 'newCounter'); //so we can now call c1 = newCounter() in lua

  luaL_newmetatable(L, 'delphi.test');
  luaL_register(L,'test', methodslib);


  //lua_pushstring(L, 'maak');
  //lua_pushcclosure(L, lua_myobject_create, 0); //assing c function counter with 1 variable containing 0 from stack


end;

var
  L: Plua_State = nil; //lua state
  script: tstringlist; //a stringlist to hold the lua script
  result: integer;     //0 if script executes ok

begin
  if ParamCount <= 0 then
  begin
    WriteLn('Usage: min.exe filename');
    Exit;
  end;

  writeln('load');
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
  lual_loadbuffer(L, script.gettext, length(script.gettext), 'myluascript');
  Script.Free; //clean up

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
