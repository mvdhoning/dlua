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

type
  my_function = function(L: Plua_State; V: Pointer): integer; cdecl;

  prop_reg = record
    Name: PChar;
    func: my_function;
    obj: Pointer;
    //offset: size_t;
  end;
  Pprop_reg = ^prop_reg;

var methods: integer;

  function lua_myobject_create(L: Plua_state): integer; cdecl; forward;

  procedure printTable(L: Plua_State);

  begin
    WriteLn('***Table***');
    lua_pushnil(L);

    while lua_next(L, -2) > 0 do
    begin
      if (lua_iscfunction(L, -1) = 1) then
      begin

        writeln(lua_tostring(L, -2) + ' = C');

      end
      else
      if (lua_isnoneornil(L, -1) = True) then //does not work?
        writeln(lua_tostring(L, -2) + ' = nil')
      else
      if (lua_isstring(L, -1)) then
        writeln(format('%s = %s', [lua_tostring(L, -2), lua_tostring(L, -1)]))
      else if (lua_isnumber(L, -1) = 1) then
        writeln(format('%s = %d', [lua_tostring(L, -2), lua_tonumber(L, -1)]))
      else if (lua_istable(L, -1)) then
      begin
        writeln('-->');
        PrintTable(L);
        writeln('<--');
      end;
      //need more types (e.g. ctypes)
      //also sometimes crashes...
      lua_pop(L, 1);
    end;
    WriteLn('***END***');
  end;

  procedure stackDump(L: Plua_State; Level: integer = 0);
  var
    i: integer;
    top: integer;
    t: integer;
  begin
    WriteLn('===StackDump===');
    top := lua_gettop(L);
    writeln(IntToStr(top));
    for i := 0 to top do
    begin // repeat for each level
      Write(IntToStr(i) + ': ');
      t := lua_type(L, i);
      case (t) of

        LUA_TSTRING:  // strings
          writeln(lua_tostring(L, i));

        LUA_TNUMBER:  // numbers
          writeln(lua_tonumber(L, i));

        LUA_TTABLE:
        begin
          Write('table ==>');
          if i = 0 then
            lua_pushnil(L);
          printTable(L); //table
          writeln('<==');
        end;

        else  // other values
          Writeln(lua_typename(L, t));
      end;
      //write('  ');  // put a separator
    end;
    writeln();  // end the listing
    WriteLn('===End===');
  end;

  //function to print lua data via delphi
  function lua_print(L: Plua_State): integer; cdecl;
  var
    i, n: integer;
  begin
    Write('Lua: ');
    n := lua_gettop(L);
    Write('n: ' + IntToStr(n));
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
    FText: string;
    FLees: string;
  public
    procedure dosomething(t: string);
    procedure settext(t: string);
    procedure showtext();
    function getLees(): string;
    procedure setLees(atext: string);
    property Text: string read ftext write ftext;
    property Lees: string read getLees write setLees;
  end;
  PMyObject = ^TMyObject;

const
  PosMetaTaleLuaTMyObject = 'metatables.LuaTMyObject';
  PosLuaTMyObject = 'tMyObject';

//var
//  methods, metatable: integer;

  procedure tmyobject.dosomething(t: string);
  begin
    ftext := t;
    writeln('DELPHI: ' + ftext); //show debug info
  end;

  procedure tmyobject.settext(t: string);
  begin
    ftext := t;
    //writeln('DELPHI: '+ftext); //show debug info
  end;

  procedure tmyobject.showtext();
  begin
    writeln('DELPHI: ' + ftext); //show debug info
  end;

  procedure tmyobject.setLees(atext: string);
  begin
    flees := atext;
  end;

  function tmyobject.getLees();
  begin
    Result := flees;
  end;

  // dosomething is a method receiving only 1 parameter of type String
  function lua_myobject_dosomething(L: Plua_state): integer; cdecl;
  var
    p: pointer;
    o: TMyObject;
  begin
    writeln('lua called test');
    p := nil;
    p := lua_touserdata(L, 1);
    if (p = nil) then
      writeln('no object?')
    else
    begin
      o := PMyObject(p)^;
      o.DoSomething(lua_tostring(L, 2));
    end;
    Result := 0; // no return values
  end;

  // dosomething is a method receiving only 1 parameter of type String
  function lua_myobject_settext(L: Plua_state): integer; cdecl;
  var
    p: pointer;
    o: TMyObject;
  begin
    writeln('lua called settext');
    p := nil;
    p := lua_touserdata(L, 1);
    if (p = nil) then
      writeln('no object?')
    else
    begin
      o := PMyObject(p)^;
      o.Settext(lua_tostring(L, 2));
    end;
    Result := 0; // no return values
  end;

  // dosomething is a method receiving only 1 parameter of type String
  function lua_myobject_showtext(L: Plua_state): integer; cdecl;
  var
    p: pointer;
    o: TMyObject;
  begin
    writeln('lua called showttext');
    stackDump(L);
    p := nil;
    p := lua_touserdata(L, 1);
    if (p = nil) then
      writeln('no object?')
    else
    begin
      o := PMyObject(p)^;
      o.ShowText();
    end;
    Result := 0; // no return values
  end;

  // delete __GC
  function lua_myobject_delete(L: Plua_state): integer; cdecl;
  var
    p: pointer;
    o: TMyObject;
  begin
    writeln('lua called __gc');

    p := nil;
    p := lua_touserdata(L, 1);
    if (p = nil) then
      writeln('no object?')
    else
    begin
      o := PMyObject(p)^;
      writeln(o.getLees());
      o.Free(); //free up object
    end;

    Result := 0; // no return values
  end;

  function get_int(L: Plua_state; v: Pointer): integer; cdecl;
  begin
    lua_pushnumber(L, double(v^));
    Result := 1;
  end;

  function get_string(L: Plua_state; v: Pointer): integer; cdecl;
  var
    o: TMyObject;
  begin
    writeln('called getstring');
    o := PMyObject(v)^;
    lua_pushstring(L, pchar(o.getLees())); //for now just return this string
    Result := 1;
  end;

  function index_handler(L: Plua_state): integer cdecl;
  var
    propname: PChar;
    prop: PProp_reg;
    ud: pointer;
  begin
    writeln('index_handler');
    //stackDump(L);

    lua_getmetatable(L, -1);


      //test to try to get the object
    if lua_type(L, -1) = LUA_TTABLE then begin
      writeln('trying to get __self');
      lua_getfield(L, 2, '__self');
      ud := luaL_checkudata(L, 2, 'metatables.LuaTMyObject');
    end;

    // stack has userdata, index
    lua_pushvalue(L, 2);                     // dup index
    lua_rawget(L, lua_upvalueindex(1));      // lookup member by name
    propname := lua_tostring(L, 2);
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

    prop := PProp_reg(lua_touserdata(L, -1));  // member info
    if prop <> nil then begin
    writeln('prop info: '+prop^.Name);

    lua_pop(L, 1);// drop lightuserdata
    lua_pushvalue(L, 2);                   // dup index */
    if lua_type(L, 1) = LUA_TUSERDATA then
    begin
      //return m->func(L, (void *)((char *)lua_touserdata(L, 1) + m->offset));
       prop^.func(L,prop^.obj); //call the registered function for this property
    end;
    end;


     (*
    if propname = 'lees' then
    begin
      lua_pop(L, 1);                         // drop value */
      lua_pushvalue(L, 2);                   // dup index */
      //lua_pushnumber(L, 7); //always return 7
      lua_pushstring(L, 'hallo uit pascal voor property');
    end;
     *)

    if propname = 'probeer' then
    begin
      lua_pop(L, 1);                         // drop value */
      lua_pushvalue(L, 2);                   // dup index */
      //lua_pushnumber(L, 7); //always return 7
      lua_pushstring(L, 'hallo uit pascal voor property');
    end;

    Result := 1;
  end;

  function newindex_handler(L: Plua_State): integer; cdecl;
  var
    propname: PChar;
    n: integer;
        prop: PProp_reg;
            o: TMyObject;
  begin
    writeln('newindex_handler');
    // stack has userdata, index, value
    lua_pushvalue(L, 2);                     // dup index
    lua_rawget(L, lua_upvalueindex(1));      // lookup member by name
    propname := lua_tostring(L, 2);
    writeln(propname);
    //if (!lua_islightuserdata(L, -1))         /* invalid member */
    //  luaL_error(L, "cannot set member '%s'", lua_tostring(L, 2));
    //return Xet_call(L);                      /* call set function */

    prop := PProp_reg(lua_touserdata(L, -1));  // member info
    if prop <> nil then begin
    writeln('prop info: '+prop^.Name);

    lua_pop(L, 1);// drop lightuserdata
    lua_pushvalue(L, 2);                   // dup index */
    if lua_type(L, 1) = LUA_TUSERDATA then
    begin
           lua_pop(L, 1);                               // drop
      n := lua_gettop(L);
       //prop^.func(L,prop^.obj); //call the registered function for this property
      o := PMyObject(prop^.obj)^;
      o.setLees(lua_tostring(L, n));
      writeln('tried to set: ' + lua_tostring(L, n));

    end;
    end;

      (*
    if propname = 'lees' then
    begin
      lua_pop(L, 1);                               // drop
      n := lua_gettop(L);
      writeln('tried to set: ' + lua_tostring(L, n));



    end;
    *)
    Result := 1;
  end;

//structure for delphi array (class) to lua
const
//class methods
methodslib: array [0..2] of luaL_reg = (
  (Name: 'new'; func: lua_myobject_create),
  (Name: 'test2'; func: lua_myobject_dosomething),
  (Name: nil; func: nil)
  );
//object methods
meta_methods: array [0..4] of luaL_reg = (
  (Name: 'test'; func: lua_myobject_dosomething),
  (Name: 'settext'; func: lua_myobject_settext),
  (Name: 'showtext'; func: lua_myobject_showtext),
  (Name: '__gc'; func: lua_myobject_delete),
  (Name: nil; func: nil)
  );
  your_getters: array [0..2] of prop_reg = (
   (name:'lees'; func:get_string ),
   (name:'id'; func:get_int ),
   //(name:'name'; func:get_string; offset:offsetof(your_t,name) ),
   //(name:'age';  func:get_int;    offset:offsetof(your_t,age)  ),
   //(name:'x';    func:get_number; offset:offsetof(your_t,x)    ),
   //(name:'y';    func:get_number; offset:offsetof(your_t,y)    ),
   (name:nil;func:nil)
  );

  var
    objcount: integer;

  function lua_myobject_create(L: Plua_state): integer; cdecl;
  var
    a: PMyObject;
    i, meta: Integer;
  begin
    writeln('lua called create');
    stackDump(L);

       //add methods (class methods)
    lua_newtable(L);
    luaL_register(L, pchar(PosLuaTMyObject+pchar(inttostr(objcount))), methodslib);
    methods := lua_gettop(L);
    //stackDump(L); //debug lua c api stack visualizer

    writeln('b');
    //add meta methods (object methods)
    //lua_getfield(L, LUA_REGISTRYINDEX, PosMetaTaleLuaTMyObject);
    luaL_newmetatable(L, pchar(PosMetaTaleLuaTMyObject+pchar(inttostr(objcount))));
    //luaL_newmetatable(L, pchar(PosMetaTaleLuaTMyObject));
    luaL_register(L, nil, meta_methods);
    meta := lua_gettop(L);



    //new(a);
    a := lua_newuserdata(L, SizeOf(TMyObject)); //assign memory for object to lua
    a^ := TMyObject.Create(); //create the object

    a^.Lees:='lees from object';
    objcount:=objcount+1;
    writeln('object teller');
    writeln(inttostr(objcount));
 (*
    //lua_pushnil(L);
    lua_getfield(L, LUA_REGISTRYINDEX, PosMetaTaleLuaTMyObject);
    //meta:=lua_gettop(L);
    //stackDump(L);
    lua_setmetatable(L, -2);
 *)

   (*
    meta := 1;


    // Do function lookups in metatable
    //lua_pushvalue(L,1);
    //lua_setfield(L, 1, '__index');

    writeln('lua add properties');

    //lua_pushliteral(L, '__metatable');
    //lua_pushvalue(L,0);    // dup methods table
    //lua_rawset(L, meta); // hide metatable: metatable.__metatable = methods
    //stackDump(L);

    lua_pushliteral(L, '__index');
    lua_pushvalue(L, meta); // upvalue index 1
    stackDump(L);

    //add properties
    for i:=0 to length(your_getters)-1 do
    begin
      if your_getters[i].name<>nil then
      begin
        writeln('1add prop: '+your_getters[i].name);
        lua_pushstring(L, your_getters[i].name); //add name
        writeln('2add prop: '+your_getters[i].name);
        lua_pushlightuserdata(L, @your_getters[i]); //add property record for name
        //lua_pushnil(L);
        writeln('3add prop: '+your_getters[i].name);

        lua_settable(L, 1);
        writeln('4add prop: '+your_getters[i].name);
      end;
    end;
    //end add properties
    writeln('done adding properties');
    //lua_pushvalue(L, methods); // upvalue index 2
    lua_pushcclosure(L, index_handler, 2); //also gets called for normal methods also?
    lua_rawset(L, meta); // metatable.__index = index_handler

    stackDump(L);
    lua_setmetatable(L, 0);
    writeln('Done with create');
    *)



        writeln('a');
    (*

    //add methods (class methods)
    lua_newtable(L);
    luaL_register(L, pchar(PosLuaTMyObject+pchar(inttostr(objcount))), methodslib);
    methods := lua_gettop(L);
    //stackDump(L); //debug lua c api stack visualizer

    writeln('b');
    //add meta methods (object methods)
    //lua_getfield(L, LUA_REGISTRYINDEX, PosMetaTaleLuaTMyObject);
    luaL_newmetatable(L, pchar(PosMetaTaleLuaTMyObject+pchar(inttostr(objcount))));
    //luaL_newmetatable(L, pchar(PosMetaTaleLuaTMyObject));
    luaL_register(L, nil, meta_methods);
    meta := lua_gettop(L);
    //stackDump(L); //debug lua c api stack visualizer

    writeln('c');
     *)
    lua_pushliteral(L, '__metatable');
    lua_pushvalue(L, methods);    // dup methods table
    lua_rawset(L, meta); // hide metatable: metatable.__metatable = methods

    writeln('d');

    //TODO: hoe voorkomen dat index_handler ook voor alle __index methods aangeroepen wordt.
    //setters (mixed with metamethods)

    //only set meta methods
    //lua_pushliteral(L, '__index');
    //lua_pushvalue(L, meta); // upvalue index 1
    //lua_rawset(L, meta); // metatable.__index = index_handle


    //luaL_newmetatable(L, 'properties');
    //props := lua_gettop(L);

    //set meta methods and properties  move this to create function?  http://loadcode.blogspot.nl/2007/02/wrapping-c-classes-in-lua.html
    lua_pushliteral(L, '__index');
    lua_pushvalue(L, meta); // upvalue index 1

    writeln('indexstackdump');
    stackDump(L);

    //add properties
    for i:=0 to length(your_getters)-1 do
    begin
      if your_getters[i].name<>nil then begin
        writeln('add prop: '+your_getters[i].name);
        your_getters[i].obj:=a;
        lua_pushstring(L, your_getters[i].name); //add name
        lua_pushlightuserdata(L, @your_getters[i]); //add property record for name
        lua_settable(L, -3);
      end;
    end;
    //end add properties

    lua_pushvalue(L, methods); // upvalue index 2
    lua_pushcclosure(L, index_handler, 2); //also gets called for normal methods also?
    lua_rawset(L, meta); // metatable.__index = index_handler

    //properties seperated this easy: http://stackoverflow.com/questions/20332518/lua-c-index-is-always-invoked-even-the-tables-field-is-known
    // lua_pushliteral(L, '__index');
    // lua_pushvalue(L, meta); // upvalue index 1
    // lua_rawset(L, meta); // metatable.__index = index_handle


    writeln('e');
    //getters
    lua_pushliteral(L, '__newindex');
    lua_newtable(L);              // table for members you can set

    //add properties

    for i:=0 to length(your_getters)-1 do
    begin
      if your_getters[i].name<>nil then begin
        writeln('add prop: '+your_getters[i].name);
        your_getters[i].obj:=a;
        lua_pushstring(L, your_getters[i].name); //add name
        lua_pushlightuserdata(L, @your_getters[i]); //add property record for name
        lua_settable(L, -3);
      end;
    end;


    //end add properties
    //writeln('newindexstackdump');
    //stackDump(L); //debug lua c api stack visualizer

    lua_pushcclosure(L, newindex_handler, 1);

    lua_rawset(L, meta);     // metatable.__newindex = newindex_handler
    //stackDump(L); //debug lua c api stack visualizer


    lua_pop(L, 1);       // drop metatable



    //lua_getfield(L, LUA_REGISTRYINDEX, PosMetaTaleLuaTMyObject);
    lua_getfield(L, LUA_REGISTRYINDEX, pchar(PosMetaTaleLuaTMyObject+pchar(inttostr(objcount))));
    writeln('object crated');
    //stackDump(L); //debug lua c api stack visualizer
    lua_setmetatable(L, -2);

    Result:=1;
  end;






  procedure registerwithlua(L: Plua_State);
  var
    MetaTable, MethodTable{, Methods}, Meta, props: integer;
    level,i: integer;
    info: Plua_debug;
  begin
    writeln('a');
      objcount:=0;

    //add methods (class methods)
    lua_newtable(L);
    luaL_register(L, PosLuaTMyObject, methodslib);
    methods := lua_gettop(L);
    //stackDump(L); //debug lua c api stack visualizer

    writeln('b');
    //add meta methods (object methods)
    luaL_newmetatable(L, PosMetaTaleLuaTMyObject);
    luaL_register(L, nil, meta_methods);
    meta := lua_gettop(L);
    //stackDump(L); //debug lua c api stack visualizer
    (*
    writeln('c');

    lua_pushliteral(L, '__metatable');
    lua_pushvalue(L, methods);    // dup methods table
    lua_rawset(L, meta); // hide metatable: metatable.__metatable = methods

    writeln('d');

    //TODO: hoe voorkomen dat index_handler ook voor alle __index methods aangeroepen wordt.
    //setters (mixed with metamethods)

    //only set meta methods
    //lua_pushliteral(L, '__index');
    //lua_pushvalue(L, meta); // upvalue index 1
    //lua_rawset(L, meta); // metatable.__index = index_handle


    //luaL_newmetatable(L, 'properties');
    //props := lua_gettop(L);

    //set meta methods and properties  move this to create function?  http://loadcode.blogspot.nl/2007/02/wrapping-c-classes-in-lua.html
    lua_pushliteral(L, '__index');
    lua_pushvalue(L, meta); // upvalue index 1

    writeln('indexstackdump');
    stackDump(L);

    //add properties
    for i:=0 to length(your_getters)-1 do
    begin
      if your_getters[i].name<>nil then begin
        writeln('add prop: '+your_getters[i].name);
        lua_pushstring(L, your_getters[i].name); //add name
        lua_pushlightuserdata(L, @your_getters[i]); //add property record for name
        lua_settable(L, -3);
      end;
    end;
    //end add properties

    lua_pushvalue(L, methods); // upvalue index 2
    lua_pushcclosure(L, index_handler, 2); //also gets called for normal methods also?
    lua_rawset(L, meta); // metatable.__index = index_handler

    //properties seperated this easy: http://stackoverflow.com/questions/20332518/lua-c-index-is-always-invoked-even-the-tables-field-is-known
    // lua_pushliteral(L, '__index');
    // lua_pushvalue(L, meta); // upvalue index 1
    // lua_rawset(L, meta); // metatable.__index = index_handle


    writeln('e');
    //getters
    lua_pushliteral(L, '__newindex');
    lua_newtable(L);              // table for members you can set

    //add properties
    lua_pushstring(L, 'lees'); //add name
    lua_pushstring(L, nil); //add userdata (for now nothing)
    lua_settable(L, -3);
    //end add properties
    //writeln('newindexstackdump');
    //stackDump(L); //debug lua c api stack visualizer

    lua_pushcclosure(L, newindex_handler, 1);

    lua_rawset(L, meta);     // metatable.__newindex = newindex_handler
    //stackDump(L); //debug lua c api stack visualizer

    lua_pop(L, 1);       // drop metatable

    writeln('f');

    //stackDump(L);
    *)
  end;

var
  L: Plua_State = nil; //lua state
  script: TStringList; //a stringlist to hold the lua script
  Result: integer;     //0 if script executes ok

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
  script := TStringList.Create;
  script.LoadFromFile(PChar(ParamStr(1)));
  lual_loadbuffer(L, script.gettext, length(script.gettext), 'myluascript');
  Script.Free; //clean up

  writeln('run lua script');
  //Ask Lua to run our little script
  Result := 0;
  Result := lua_pcall(l, 0, LUA_MULTRET, 0); //TODO: reimplment
  if Result > 0 then
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
