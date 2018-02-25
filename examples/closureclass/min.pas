program min;

{$APPTYPE CONSOLE}

// defines to configure freepascal
{$IFDEF FPC}
  {$MODE Delphi}
 {$M+}

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
      fmyvar: String;
    public
      //function Show(): String;
    published
      function Show(): String;
      function Merge(avalue: string): String;
      property MyString: String read fmystring write fmystring;
      property MyVar: String read fmyvar write fmyvar;
  end;
  PMyClass = ^TMyClass;

  prop_reg = record
    Name: PChar;
    funcget: lua_CFunction;
    funcset: lua_CFunction;
  end;

function TMyClass.Show(): String;
begin
  Result := 'Show: '+fmyvar;
end;

function TMyClass.Merge(avalue: string): String;
begin
  Result := 'Merge: '+avalue;
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

function lua_myclass_string_function(L: Plua_State): Integer; cdecl;
var
  p: Pointer;
  o: TMyClass;
  f,r,v: string;
  n: Integer;
begin
  writeln('lua called method');

  n:= lua_gettop(L);
  writeln('n: '+inttostr(n));

  p := nil;
  p := lua_touserdata(L, 1); //get a pointer to the pascal object
  if (p = nil) then
    writeln('no object?')
  else
  begin
    o := PMyClass(p)^; //get the object

    f := lua_tostring(L, 2); //get the method name
    writeln('Method: '+f);

    //For this 'shared' wrapper function we need to call the right function in the object by name
    if f = 'Show' then
      begin
       r:=o.Show();
      end
    else if f = 'Merge' then
       begin
         v := lua_tostring(L, 3); //get the property value
         writeln('Param1: '+v);
         r:=o.Merge(v);
       end;

    writeln('Result: '+r);
    lua_pushstring(L, pchar(r)); //return the result of o.show as string to lua
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
  p := lua_touserdata(L, 1); //get a pointer to the pascal object
  if (p = nil) then
    writeln('no object?')
  else
  begin

    f := lua_tostring(L, 2); //get the property name
    writeln('Prop: '+f);

    o := PMyClass(p)^; //get the object
    lua_pushstring(L, pchar(GetStrProp(o,f))); //get the value from propertyname at object
  end;

  Result:=1;
end;

function lua_myclass_set_string(L: Plua_State): Integer; cdecl;
var
  p: Pointer;
  o: TMyClass;
  f: string;
  v: string;
begin
  writeln('lua called set string');

  p := nil;
  p := lua_touserdata(L, 1); //get a pointer to the pascal object
  if (p = nil) then
    writeln('no object?')
  else
  begin
    f := lua_tostring(L, 2); //get the property name
    writeln('Prop: '+f);

    v := lua_tostring(L, 3); //get the property value
    writeln('Value: '+v);

    o := PMyClass(p)^; //get the object
    SetStrProp(o,f,v); //set the value for property name in object

  end;

  Result:=1;
end;

var
  //object properties
  myclass_properties: array [0..2] of prop_reg = (
   (name:'MyString'; funcget:lua_myclass_get_string; funcset:lua_myclass_set_string),
   (name:'MyVar'; funcget:lua_myclass_get_string; funcset:lua_myclass_set_string),
   (name:nil;funcget:nil;funcset:nil)
  );
  myclass_methods: array [0..2] of luaL_reg = (
  (Name: 'Show'; func: lua_myclass_string_function),
  (Name: 'Merge'; func: lua_myclass_string_function),
  (Name: nil; func: nil)
  );
  myclass_constructors: array [0..1] of luaL_reg = (
  (Name: 'Create'; func: lua_myclass_create),
  (Name: nil; func: nil)
  );
  myclass_destructors: array [0..1] of luaL_reg = (
  (Name: 'Free'; func: lua_myclass_free),
  (Name: nil; func: nil)
  );

function lua_myclass_loader(L: Plua_State):Integer; cdecl;
var
  name: string;
  script: TStringList;
  pscript: pchar;
  s,i: Integer;
begin
  writeln('lua called myclass loader');
  //load the first parameter
  //which is the name of the file, or whatever string identifier for a resource that you passed in with require()
  name := 'T'+lua_tostring(L,1);
  writeln('name: '+name);

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

  //class name
  script.Add('-- this will be the class generated from pascal');
  script.Add('function '+name+'(...)');
  script.Add('local self = BaseClass()');

  //add call pascal constructors (only one supported for now)
  script.Add('print("hello from '+name+' constructor")');
  for i:=0 to length(myclass_constructors)-1 do
    begin
      if myclass_constructors[i].name<>nil then
      begin
        lua_register(L, pchar(name+'_'+myclass_constructors[i].name), myclass_constructors[i].func); //constructor
        script.Add('self._pascalclass = '+name+'_'+myclass_constructors[i].name+'();');
      end;
    end;

  //add function to swap lua generated pascal object with object from pascal
  script.Add('function self.changeobject(v)');
  script.Add('  print("changeobject called")');
  script.Add('  TMyClass_Free(self._pascalclass)'); //free current pascal object
  script.Add('  self._pascalclass = v'); //set object from pascal
  script.Add('end');

  //add functions and procedure (no need to also use class name here)
  for i:=0 to length(myclass_methods)-1 do
    begin
      if myclass_methods[i].name<>nil then
      begin
        lua_register(L, pchar(name+'_'+myclass_methods[i].name), myclass_methods[i].func);
        script.Add('function self.'+myclass_methods[i].name+'(...)');
        script.Add('  return '+name+'_'+myclass_methods[i].name+'(self._pascalclass,"'+myclass_methods[i].name+'",...);');
        script.Add('end');
      end;
    end;

  //add functions for properties
  for i:=0 to length(myclass_properties)-1 do
    begin
      if myclass_properties[i].name<>nil then
      begin
        lua_register(L, pchar(name+'_Set_'+myclass_properties[i].name), myclass_properties[i].funcset); //call string property setter
        lua_register(L, pchar(name+'_Get_'+myclass_properties[i].name), myclass_properties[i].funcget); //call string property getter

        script.Add(' -- add setter/getter for each pascal property');
        script.Add('local function _private'+myclass_properties[i].name+'Setter(v)');
        script.Add('  '+name+'_Set_'+myclass_properties[i].name+'(self._pascalclass,"'+myclass_properties[i].name+'",v)');
        script.Add('end');
        script.Add('local function _private'+myclass_properties[i].name+'Getter()');
        script.Add('  return '+name+'_Get_'+myclass_properties[i].name+'(self._pascalclass,"'+myclass_properties[i].name+'")');
        script.Add('end');
        script.Add('-- end add setter/getter for each pascal property');

        //properties
        script.Add('self._member = { key="'+myclass_properties[i].name+'",');
        script.Add('                 set=_private'+myclass_properties[i].name+'Setter,');
        script.Add('                 get=_private'+myclass_properties[i].name+'Getter');
        script.Add('               }');

      end;
    end;

  //return the instance
  script.Add('return self');

  script.Add('end');

  script.Add('--end generated script');

  //add pascal destructor (only one supported and needs to be called Free)
  for i:=0 to length(myclass_destructors)-1 do
    begin
      if myclass_destructors[i].name<>nil then
      begin
        lua_register(L, pchar(name+'_'+myclass_destructors[i].name), myclass_destructors[i].func); //constructor
      end;
    end;

  //script.SaveToFile('generated.lua'); //uncomment to save generated script

  //load the buffer
  pscript:=script.gettext;
  s:=lual_loadbuffer(L, pscript, length(pscript), pchar(name));
  Writeln(inttostr(s)); //debug if load buffer worked
  StrDispose(pscript); //clean up
  freeAndNil(script); //clean up
  //compile the lua script so that other scripts can see it
  s:=lua_pcall (L, 0, 0, 0); //add it to the scripts
  Writeln(inttostr(s)); //debug if pcall worked
  writeln('end myclass loader');

  Result := 1;

end;

procedure lua_myclass_register_addobject(L: Plua_State);
var
  name: string;
  script: TStringList;
  pscript: pchar;
  s: Integer;
  a: PMyClass;
begin
  name:='MyClass';

  //generate our custom dynamic script
  script:=tstringList.Create;
  //script.Add('require "MyClass"');
  script.Add('print("add object script")');
  script.Add('function AddObject(v)');
  script.Add('o = T'+name+'()'); //create object
  script.Add('o.changeobject(v)'); //change to pascal object
  script.Add('return o'); //change to pascal object
  script.Add('end');

  //script.SaveToFile('addobject.lua'); //uncomment to save generated script

  //load the buffer
  pscript:=script.gettext;
  s:=lual_loadbuffer(L, pscript, length(pscript), pscript);
  Writeln('load:'+inttostr(s)); //debug if load buffer worked
  StrDispose(pscript); //clean up
  freeAndNil(script); //clean up
  s:=lua_pcall(L, 0, 0,0);     // enable script
  writeln('compile: '+inttostr(s));
end;

procedure lua_myclass_addobject(L: Plua_State; O: TMyClass; aname: string);
var
  s: Integer;
  a: PMyClass;
begin
  //call AddObject lua script to set object in lua
  lua_getglobal(L,pchar('AddObject')); //function to be called
  a:=lua_newuserdata(L, SizeOf(TMyClass)); //pass object to lua as first parameter
  a^:=o;
  s:=lua_pcall(L, 1, LUA_MULTRET,0); // call 'MyClass' with 1 arguments and 1 result
  lua_setglobal(L,pchar(aname)); // set global 'varname'

  Writeln(inttostr(s)); //debug if pcall worked
  writeln('end add object');
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
  luaL_requiref( L, 'MyClass', lua_myclass_loader, 1 ); //Register MyClass so it can be called with require "MyClass"
                                                        //lua_myclass_loader provides the content of MyClass
end;

var
  L: Plua_State = nil; //lua state
  script: tstringlist; //a stringlist to hold the lua script
  result: integer;     //0 if script executes ok
  test, mytest: TMyClass;
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


  writeln('register print');

  //Register a delphi procedure/funtion for use in Lua
  lua_register(L, 'print', lua_print);

  //register pseudo delphi array (class) in lua
  registerwithlua(L);
  lua_myclass_register_addobject(L);

  test := TMyClass.Create();
  test.MyVar:='this is set in pascal';
  lua_myclass_addobject(L, test, 'test'); //add object test as test in lua

  mytest := TMyClass.Create();
  mytest.MyVar:='this is mytest';
  lua_myclass_addobject(L, mytest, 'mytest'); //add object test as test in lua

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

  writeln('test.MyString: '+test.MyString);
  //test.free(); //Do not call as this is still registered with lua

  //close lua dll
  lua_close(L); //this will also unload registered pascal objects
  UnLoadLua;

  Writeln('press [ENTER] key to exit...');
  ReadLn;

end.
