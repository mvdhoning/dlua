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

function get_int (L: Plua_state; v: Pointer): Integer; cdecl;
begin
  lua_pushnumber(L, double(v^));
  Result:=1;
end;

function index_handler (L: Plua_state): Integer cdecl;
var
  propname:pchar;
begin
  writeln('index_handler');
  // stack has userdata, index
  lua_pushvalue(L, 2);                     // dup index
  lua_rawget(L, lua_upvalueindex(1));      // lookup member by name
  propname:=lua_tostring(L, 2);
  writeln(propname);
  (*
  if (!lua_islightuserdata(L, -1)) {
    lua_pop(L, 1);                         /* drop value */
    lua_pushvalue(L, 2);                   /* dup index */
    lua_gettable(L, lua_upvalueindex(2));  /* else try methods */
    if (lua_isnil(L, -1))                  /* invalid member */
      luaL_error(L, "cannot get member '%s'", lua_tostring(L, 2));
    return 1;
  }
  return Xet_call(L);                      /* call get function */
  *)
  if propname='lees' then
    begin
  lua_pop(L, 1);                         // drop value */
  lua_pushvalue(L, 2);                   // dup index */
  //lua_pushnumber(L, 7); //always return 7
  lua_pushstring(L,'ddd');
  end;
  Result := 1;
end;

function newindex_handler (L: Plua_State): Integer; cdecl;
var
  propname:pchar;
  n:integer;
begin
  writeln('newindex_handler');
  // stack has userdata, index, value
  lua_pushvalue(L, 2);                     // dup index
  lua_rawget(L, lua_upvalueindex(1));      // lookup member by name
  propname:=lua_tostring(L, 2);
  writeln(propname);
  //if (!lua_islightuserdata(L, -1))         /* invalid member */
  //  luaL_error(L, "cannot set member '%s'", lua_tostring(L, 2));
  //return Xet_call(L);                      /* call set function */
  if propname='lees' then
    begin
     lua_pop(L, 1);                               // drop
     n := lua_gettop(L);
     writeln('tried to set: '+lua_tostring(L,n));
    end;
  result:=1;
end;

type
  my_function = function(L: Plua_State; V: Pointer): integer; cdecl;

  prop_reg = record
    Name: PChar;
    func: my_function;
    offset: size_t;
  end;
  Pprop_reg = ^prop_reg;

//structure for delphi array (class) to lua
const
  //class methods
  methodslib: array [0..2] of luaL_reg = (
   (name:'new';func:lua_myobject_create),
   (name:'test2';func:lua_myobject_dosomething),
   (name:nil;func:nil)
   );
  //object methods
  meta_methods: array [0..4] of luaL_reg = (
   (name:'test';func:lua_myobject_dosomething),
   (name:'settext';func:lua_myobject_settext),
   (name:'showtext';func:lua_myobject_showtext),
   (name:'__gc';func:lua_myobject_delete),
   (name:nil;func:nil)
   );
  (*
  your_getters: array [0..1] of prop_reg = (
   (name:'id';   func:get_int;    offset:@test   ),
   //(name:'name'; func:get_string; offset:offsetof(your_t,name) ),
   //(name:'age';  func:get_int;    offset:offsetof(your_t,age)  ),
   //(name:'x';    func:get_number; offset:offsetof(your_t,x)    ),
   //(name:'y';    func:get_number; offset:offsetof(your_t,y)    ),
   (name:nil;func:nil;offset:nil)
  );
  *)

procedure registerwithlua(L: Plua_State);
var
 MetaTable,
 MethodTable,
 Methods, Meta : Integer;
begin
  writeln('a');

  //add methods (class methods)
  lua_newtable(L);
  luaL_register(L,PosLuaTMyObject, methodslib);
  methods := lua_gettop(L);

  writeln('b');
  //add meta methods (object methods)
  luaL_newmetatable(L, PosMetaTaleLuaTMyObject);
  luaL_register(L, Nil, meta_methods);
  meta := lua_gettop(L);

  writeln('c');

  lua_pushliteral(L, '__metatable');
  lua_pushvalue(L, methods);    // dup methods table
  lua_rawset(L, meta); // hide metatable: metatable.__metatable = methods

  writeln('d');
  //setters (mixed with metamethods)
  lua_pushliteral(L, '__index');
  lua_pushvalue(L, meta); // upvalue index 1

  //add properties
  lua_pushstring(L, 'lees'); //add name
  lua_pushstring(L,nil); //add userdata (for now nothing)
  lua_settable(L, -3);
  //end add properties

  lua_pushvalue(L, methods); // upvalue index 2

  lua_pushcclosure(L, index_handler, 2);
  lua_rawset(L, meta); // metatable.__index = index_handler

  writeln('e');
  //getters
  lua_pushliteral(L, '__newindex');
  lua_newtable(L);              // table for members you can set

  //add properties
  lua_pushstring(L, 'lees'); //add name
  lua_pushstring(L,nil); //add userdata (for now nothing)
  lua_settable(L, -3);
  //end add properties

  lua_pushcclosure(L, newindex_handler, 1);
  lua_rawset(L, meta);     // metatable.__newindex = newindex_handler

  lua_pop(L, 1);       // drop metatable

  writeln('f');

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
