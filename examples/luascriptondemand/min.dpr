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

procedure registerwithlua(L: Plua_State);
begin
 //register with lua
 
 //idea on loading generated lua file on demand
    //url: http://www.gamedev.net/topic/661707-solved-lua-require-other-files-when-loaded-from-memory/
    //or just add it in front of the normal loaded lua file ...
    //example code:
    (*

    int MyLoader(lua_State *L){
	//load the first parameter
	//which is the name of the file, or whatever string identifier for a resource that you passed in with require()
	string filename = lua_tostring(state,1);

	if( exist_in_container(filename)){
		buffer = get_data_from_container(filename);

		//load the buffer
		luaL_loadbuffer(L,buffer.data,buffer.size, filename.c_str());

		return 1;
	}else{

		lua_pushstring(L, "Error: file not found");
		return 1;
	}
}

// when you initialize your lua state...

//locate the package.loaders table
lua_getfield(state,LUA_GLOBALSINDEX,"package");
lua_getfield(state,-1,"loaders");
lua_remove(state,-2);


//for convienice, we just replace the first one with our loader
//package.loaders[1] = MyLoader
lua_pushinteger(state,1);
lua_pushcfunction(state,MyLoader);
lua_rawset(state,-3);
//balance the stack.
lua_pop(state,1);

    *)
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
