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
  lua;

//function to print lua data via delphi
function lua_print(L: Plua_State): Integer; cdecl;
var
  i, n: Integer;
begin
  write('Lua: ');
  n := lua_gettop(L);
  write('n: '+inttostr(n));
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

//this is the class we want to use from lua
type
  TMyObject = class
  private
    FText : string;
  public
    procedure dosomething(t: string);
    procedure settext(t: string);
    procedure showtext();
    property text : string read ftext write ftext;
  end;
  PMyObject = ^TMyObject;

Const
  PosMetaTaleLuaTMyObject = 'metatables.LuaTMyObject';
  PosLuaTMyObject = 'tMyObject';

var
  methods, metatable: integer;

procedure tmyobject.dosomething(t: string);
begin
  ftext:=t;
  writeln('DELPHI: '+ftext); //show debug info
end;

procedure tmyobject.settext(t: string);
begin
  ftext:=t;
  //writeln('DELPHI: '+ftext); //show debug info
end;

procedure tmyobject.showtext();
begin
  writeln('DELPHI: '+ftext); //show debug info
end;

function lua_myobject_create(L: Plua_state): Integer; cdecl;
var
  a: PMyObject;
begin
  //http://stackoverflow.com/questions/8340399/how-to-register-lua-userdata-correct-from-delphi
  //http://lua-users.org/wiki/ObjectOrientationTutorial

  writeln('lua called create');
  //lua_boxpointer(L, Pointer(TMyObject.Create));
  a:=lua_newuserdata(L, SizeOf(TMyObject)); //assign memory for object to lua
  a^:=TMyObject.Create(); //create the object
  writeln('2');
  lua_getfield(L, LUA_REGISTRYINDEX, PosMetaTaleLuaTMyObject);
  lua_setmetatable(L, -2);
  writeln('3');
               (*
  //lua_getmetatable(L,metatable);
  //luaL_getmetatable(L, 'LuaBook.tMyObject');
  luaL_newmetatable(L, 'tMyObject'); //just assume it exists
   writeln('3');
  //lua_pushvalue(L, lua_upvalueindex(1));
  writeln('4');
  lua_setmetatable(L, -2);
  writeln('5');
  *)
  Result:= 1;
end;

// dosomething is a method receiving only 1 parameter of type String
function lua_myobject_dosomething(L: Plua_state): Integer; cdecl;
var
  p: pointer;
  o: TMyObject;
begin
  writeln('lua called test');
  p:=nil;
  p:= lua_touserdata(L, 1);
  if (p = nil) then writeln('no object?') else
    begin
     o:= PMyObject(p)^;
     o.DoSomething(lua_tostring(L, 2));
    end;
  Result:= 0; // no return values
end;

// dosomething is a method receiving only 1 parameter of type String
function lua_myobject_settext(L: Plua_state): Integer; cdecl;
var
  p: pointer;
  o: TMyObject;
begin
  writeln('lua called settext');
  p:=nil;
  p:= lua_touserdata(L, 1);
  if (p = nil) then writeln('no object?') else
    begin
     o:= PMyObject(p)^;
     o.Settext(lua_tostring(L, 2));
    end;
  Result:= 0; // no return values
end;

// dosomething is a method receiving only 1 parameter of type String
function lua_myobject_showtext(L: Plua_state): Integer; cdecl;
var
  p: pointer;
  o: TMyObject;
begin
  writeln('lua called showttext');
  p:=nil;
  p:= lua_touserdata(L, 1);
  if (p = nil) then writeln('no object?') else
    begin
     o:= PMyObject(p)^;
     o.ShowText();
    end;
  Result:= 0; // no return values
end;

// delete __GC
function lua_myobject_delete(L: Plua_state): Integer; cdecl;
var
  p: pointer;
  o: TMyObject;
begin
  writeln('lua called __gc');

  p:=nil;
  p:= lua_touserdata(L, 1);
  if (p = nil) then writeln('no object?') else
    begin
     o:= PMyObject(p)^;
     o.Free(); //free up object
    end;

  result:= 0; // no return values
end;

//structure for delphi array (class) to lua
const
  methodslib: array [0..2] of luaL_reg = (
   (name:'new';func:lua_myobject_create),
   (name:'test2';func:lua_myobject_dosomething),
   (name:nil;func:nil)
   );
  meta_methods: array [0..4] of luaL_reg = (
   (name:'test';func:lua_myobject_dosomething),
   (name:'settext';func:lua_myobject_settext),
   (name:'showtext';func:lua_myobject_showtext),
   (name:'__gc';func:lua_myobject_delete),
   (name:nil;func:nil)
   );

procedure registerwithlua(L: Plua_State);
var
 MetaTable,
 MethodTable,
 Methods : Integer;
begin
  writeln('a');
  luaL_newmetatable(L, PosMetaTaleLuaTMyObject);
  // Metatable.__index = Metatable
  writeln('b');
  lua_pushvalue(L, -1);
  writeln('c');
  lua_setfield(L, -2, '__index');
  writeln('d');
  luaL_register(L, Nil, meta_methods);
  writeln('e');
  luaL_register(L,PosLuaTMyObject, methodslib);
  writeln('f');
  (*
  //http://lua-users.org/wiki/BindingWithMembersAndMethods
  //http://www.pascalgamedevelopment.com/showthread.php?2810-using-Delphi-classes-in-lua-)/page2
  writeln('1');
  if (@luaL_openlib = nil) then write('Oepsie1');
  if (@luaL_newmetatable = nil) then write('Oepsie2');
  if (@luaL_setmetatable = nil) then write('Oepsie3');
  luaL_newmetatable(L, 'LuaBook.tMyObject'); //leaves new metatable on the stack
  writeln('2');
  lua_pushvalue(L, -1); // there are two 'copies' of the metatable on the stack
  writeln('3');
  lua_setfield(L, -2, '__index'); // pop one of those copies and assign it to
                                  // __index field od the 1st metatable
  writeln('4');
  if (@luaL_register = nil) then write('Oepsie4');
  luaL_register(L, nil, meta_methods); // register functions in the metatable
  //luaL_register(L, nil, methodslib); // register functions in the metatable
  writeln('5');
  luaL_register(L, 'tMyObject', methodslib);
  writeln('6');
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

  writeln('load');
  //init lua dll
  LoadLua;
  LoadLuaLib;
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
  result := lua_pcall(l, 0, LUA_MULTRET, 0);
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
  UnLoadLuaLib;

  Writeln('press [ENTER] key to exit...');
  ReadLn;

end.
