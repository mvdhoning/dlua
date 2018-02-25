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

function my_index(L: Plua_state): integer; cdecl;
var
  propname: pchar;
begin
  (*
  Result := 0; //return 0 by default (nothing found)
  writeln('my index');

  lua_getglobal(L, 'mytable');
  lua_pushvalue(L, 2);
  lua_gettable(L, -2);
  if (lua_isnil(L, -1)=false) then
        Result := 1; //we have a result so return 1
  *)

  lua_getmetatable(L, -1);
  writeln('test') ;
  // stack has userdata, index
  lua_pushvalue(L, 2);                     // dup index
  lua_rawget(L, lua_upvalueindex(1));      // lookup member by name
  propname := lua_tostring(L, 2);
  writeln(propname);
  lua_pushstring(L,lua_touserdata(L, -1));  // member info
  result:=1;
end;

procedure registerwithlua(L: Plua_State);
begin
  // create the global 'mytable' table and use a C function as the __index metamethod
  lua_createtable(L, 0, 2); //table with 2 elements

  //add data
  lua_pushstring(L, '3'); //value
  lua_setfield(L, -2, 'three'); //key

  lua_pushstring(L, '4'); //value
  lua_setfield(L, -2, 'four'); //key

  //add the metatable
  lua_createtable(L, 0, 1); //with one element __index that points to the my index function
  lua_pushcfunction(L, my_index);
  lua_setfield(L, -2, '__index');
  lua_setmetatable(L, -2);  //make this last table a metatable

  //we are back at the previous table with data
  lua_setglobal(L, 'mytable'); //give this table a name
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
