program min;

{$APPTYPE CONSOLE}

// defines to configure freepascal
{$IFDEF FPC}
  {$MODE Delphi}
// {$M+}

  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

uses
  SysUtils,
  Classes,
  dlua,
  TypInfo;

//pascal class for use in lua

type
  TMyClass= class
    private
      fmystring: String;
    public
      function Show(): String;
    published
      property MyString: String read fmystring write fmystring;
  end;
  PMyClass = ^TMyClass;

function TMyClass.Show();
begin
  Result := 'Show: '+fmystring;
end;

//helpers

function lua_myclass_create(L: Plua_State): Integer; cdecl;
var
  a: PMyClass;
begin
  writeln('lua called create');

  a:=lua_newuserdata(L, SizeOf(TMyClass)); //assign memory for object to lua
  a^:=TMyClass.Create(); //create the object

  Result:=1;
end;

function lua_myclass_free(L: Plua_State): Integer; cdecl;
var
  p: Pointer;
  o: TMyClass;
begin
  writeln('lua called free');

  p := nil;
  p := lua_touserdata(L, 1); //get the pascal object
  if (p = nil) then
    writeln('no object?')
  else
  begin
    o := PMyClass(p)^;
    writeln(o.MyString); //show contents of mystring for debug
    o.Free();
  end;

  Result:=1;
end;

function lua_myclass_show(L: Plua_State): Integer; cdecl;
var
  p: Pointer;
  o: TMyClass;
begin
  writeln('lua called show');

  p := nil;
  p := lua_touserdata(L, 1); //get the pascal object
  if (p = nil) then
    writeln('no object?')
  else
  begin
    o := PMyClass(p)^;
    lua_pushstring(L, pchar(o.Show())); //return the result of o.show as string to lua
  end;

  Result:=1;
end;

function lua_myclass_get_string(L: Plua_State): Integer; cdecl;
var
  p: Pointer;
  o: TMyClass;
  f: String;
begin
  writeln('lua called get string');

  p := nil;
  p := lua_touserdata(L, 1); //get the pascal object
  if (p = nil) then
    writeln('no object?')
  else
  begin

    f := lua_tostring(L, 2); //get the property name
    writeln('Prop: '+f);

    o := PMyClass(p)^;
    //lua_pushstring(L, pchar(o.MyString)); //return the result of o.show as string to lua
    lua_pushstring(L, pchar(GetStrProp(o,f)));
  end;

  Result:=1;
end;

function lua_myclass_set_string(L: Plua_State): Integer; cdecl;
var
  p: Pointer;
  o: TMyClass;
  f: string;
  v: string;
  PI : PPropInfo;
begin
  writeln('lua called set string');

  p := nil;
  p := lua_touserdata(L, 1); //get the pascal object
  if (p = nil) then
    writeln('no object?')
  else
  begin
    f := lua_tostring(L, 2); //get the property name
    writeln('Prop: '+f);

    v := lua_tostring(L, 3); //get the property value
    writeln('Value: '+v);

    o := PMyClass(p)^;
    //o.MyString:=v;

    //PI:=GetPropInfo(O,'MyString');
    //Writeln('Get (propinfo)          : ',GetStrProp(o,'MyString'));
    SetStrProp(o,f,v);
    //lua_pushstring(L, pchar(o.Show())); //return the result of o.show as string to lua
  end;

  Result:=1;
end;

//end pascal class for use in lua

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
  luaL_openlibs(L); //make some standard lua things work (like require and setmetatable)

  //lua class example
  lua_register(L, 'TMyClass_Create', lua_myclass_create); //constructor
  lua_register(L, 'TMyClass_Free', lua_myclass_free); //constructor
  //functions
  lua_register(L, 'TMyClass_Show', lua_myclass_show); //call show of TMyClass
  //properties
  lua_register(L, 'TMyClass_Set_String', lua_myclass_set_string); //call string property setter
  lua_register(L, 'TMyClass_Get_String', lua_myclass_get_string); //call string property getter
  //end lua class example
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
