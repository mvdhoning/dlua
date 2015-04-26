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
  PI : PTypeInfo;
  PT : PTypeData;
  PP : PPropList;
  i: integer;
begin
  writeln('lua called create');

  a:=lua_newuserdata(L, SizeOf(TMyClass)); //assign memory for object to lua
  a^:=TMyClass.Create(); //create the object

  PI:=a^.ClassInfo;
  PT:=GetTypeData(PI);
  Writeln('Property Count : ',PT^.PropCount);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
    begin
    With PP^[i]^ do
      begin
      Writeln('Property : ',name);
      //writeln('  Type: ',TypeNames[typinfo.PropType(O,Name)]);
      writeln('    Type : ',typinfo.PropType(A^,Name));
      end;
    end;
  FreeMem(PP);

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

function lua_myclass_loader(L: Plua_State):Integer; cdecl;
var
  name: string;
  script: TStringList;
  s: Integer;
  propname1: string;
begin
  writeln('lua called myclass loader');
  //load the first parameter
  //which is the name of the file, or whatever string identifier for a resource that you passed in with require()
  name := 'T'+lua_tostring(L,1);
  writeln('name: '+name);
  propname1:= 'MyString'; //published property name

  //generate our custom dynamic script
  script:=tstringList.Create;
  script.Add('--generated script');
  script.Add('function BaseClass()');

  script.Add('local _members = { }');
  script.Add('local _pascalclass = nil;');

  script.Add('print("hello from BaseClass constructor")');

  script.Add('return setmetatable( { },');
  script.Add('{');
  script.Add('      __newindex = function(self, k, v)');
  script.Add('        if k=="_member" then');
  script.Add('              assert(type(v)=="table", "Invalid member")');
  script.Add('              assert(v.key~=nil, "Invalid member")');
  script.Add('              _members[v.key]={get=v.get, set=v.set}');
  script.Add('        elseif _members[k] == nil then');
  script.Add('              rawset(self,k,v)');
  script.Add('        else');
  script.Add('              assert(_members[k].set~=nil,"Attempt to write to a read-only member: ",k)');
  script.Add('              _members[k].set(v)');
  script.Add('        end');
  script.Add('      end,');
  script.Add('      __index = function (self, k)');
  script.Add('        if _members[k]~= nil then');
  script.Add('              if _members[k].get~=nil then');
  script.Add('                return _members[k]:get()');
  script.Add('              end');
  script.Add('        end');
  script.Add('      end,');
  script.Add('		__gc = function (self)');
  script.Add('		  TMyClass_Free(self._pascalclass);');
  script.Add('		end');
  script.Add('})');
  script.Add('end');

  script.Add('-- this will be the class generated from pascal');
  script.Add('function '+name+'()');
  script.Add('local self = BaseClass()');

  //add functions for properties
  script.Add(' -- add setter/getter for each pascal property');
  script.Add('local function _private'+propname1+'Setter(v)');
  script.Add('  '+name+'_Set_String(self._pascalclass,"'+propname1+'",v)');
  script.Add('end');
  script.Add('local function _private'+propname1+'Getter()');
  script.Add('  return '+name+'_Get_String(self._pascalclass,"'+propname1+'")');
  script.Add('end');
  script.Add('-- end add setter/getter for each pascal property');

  //add constructor
  script.Add('print("hello from '+name+' constructor")');
  script.Add('self._pascalclass = '+name+'_Create();');

  //add functions and procedure
  script.Add('function self.show()');
  script.Add('  return '+name+'_Show(self._pascalclass);');
  script.Add('end');

  //properties
  script.Add('self._member = { key="'+propname1+'",');
  script.Add('                 set=_private'+propname1+'Setter,');
  script.Add('                 get=_private'+propname1+'Getter');
  script.Add('               }');

  //return the instance
  script.Add('return self');

  script.Add('end');

  script.Add('--end generated script');

  //script.SaveToFile('generated.lua'); //uncomment to save generated script

  //load the buffer
  s:=lual_loadbuffer(L, script.gettext, length(script.gettext), pchar(name));
  Writeln(inttostr(s)); //debug if load buffer worked
  script.free;
  //compile the lua script so that other scripts can see it
  s:=lua_pcall (L, 0, 0, 0); //add it to the scripts
  Writeln(inttostr(s)); //debug if pcall worked
  writeln('end myclass loader');

  Result := 1;

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
var
  name: string;
begin
  name:='MyClass';
  //register with lua
  luaL_openlibs(L); //make some standard lua things work (like require and setmetatable)

  //lua class example
  luaL_requiref( L, pchar(name), lua_myclass_loader, 1 ); //register mymodule so it can be called with require "mymodule"
                                               //MyLoader provides the content of mymodule
  //constructor and desctuctor
  lua_register(L, pchar('T'+name+'_Create'), lua_myclass_create); //constructor
  lua_register(L, pchar('T'+name+'_Free'), lua_myclass_free); //constructor
  //functions
  lua_register(L, pchar('T'+name+'_Show'), lua_myclass_show); //call show of TMyClass
  //properties
  lua_register(L, pchar('T'+name+'_Set_String'), lua_myclass_set_string); //call string property setter
  lua_register(L, pchar('T'+name+'_Get_String'), lua_myclass_get_string); //call string property getter
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
