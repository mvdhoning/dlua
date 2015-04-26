print("hello world, from Lua!\n")

print("closure test")

--http://lua-users.org/wiki/ObjectOrientationTutorial
--http://www.computercraft.info/forums2/index.php?/topic/15013-setget-members-in-an-object-oriented-class/ (for properties)
--idea: generate below code from pascal with cfunction calls to pascal
--problem: get a pointer to the object in the self table (and hide that) think i use __hidden for that filled on create and passed along each call to pascal

local function BaseClass()
  
  local _members = { }
  local _pascalclass = nil; --pascal object storage
  
  print("hello from BaseClass constructor")
 
  -- Returns the instance
  return setmetatable( { },
  {
        __newindex = function(self, k, v)
          if k=="_member" then
                assert(type(v)=="table", "Invalid member")
                assert(v.key~=nil, "Invalid member")
                _members[v.key]={get=v.get, set=v.set}
          elseif _members[k] == nil then
                rawset(self,k,v)
          else
                assert(_members[k].set~=nil,"Attempt to write to a read-only member: ",k)
                _members[k].set(v)
          end
        end,
        __index = function (self, k)
          if _members[k]~= nil then
                if _members[k].get~=nil then
                  return _members[k]:get()
                end
          end
        end,
		__gc = function (self)
		  TMyClass_Free(self._pascalclass); --call pascal object free
		end
  })

end

-- this will be the class generated from pascal
function MyClass(init)
  --private
  local self = BaseClass()
  local _privateVariable = init --only for test
  local function _privateVariableSetter(v) --only for test
        _privateVariable = 2 * v --only for test
  end
  
  -- add setter for each pascal property
  local function _privateMyStringSetter(v)
    TMyClass_Set_String(self._pascalclass,"MyString",v) --call pascal helper with pascal object property name and value
  end
  local function _privateMyStringGetter()
    return TMyClass_Get_String(self._pascalclass,"MyString")
  end
  -- end add setter for each pascal property
  
  print("hello from MyClass constructor")
  self._pascalclass = TMyClass_Create(); --call to pascal constructor
  
  --public 
  
  --functions
  function self.foo() --lua function
    return "hello!"
  end
  function self.show() --pascal function
    return TMyClass_Show(self._pascalclass); --call with pascal object and optional parameters
  end
  
  --properties
  self._member = { key="var", --lua property
                   set=_privateVariableSetter,
                   get=function() return _privateVariable end } 
  self._member = { key="MyString", --pascal property
                   set=_privateMyStringSetter,
                   get=_privateMyStringGetter
                 }
  --return the instance
  return self
  
end
--end generated from pascal

-- start of actual lua script

function MyInheritedClass(init)
  --private
  local self = MyClass(init)
  
  function self.Test()
	print("Hello from inherited class");
  end
  
  return self
end


--[[
print("create myclass")
instance = BaseClass()
instance._member = {key="type", get=function() return "class" end}
print(instance.type) --> class
--]]

print("MyClass Tests")
ins = MyClass(5)
print(ins.foo()) --> Hello!
print(ins.var) --> 5
ins.var = 2
print(ins.var) --> 4

print(ins.show()) --> Show: 

ins.MyString = "Hoi"
print(ins.MyString)

ins2 = MyClass(3)
ins2.MyString = "Hallo"
print(ins2.MyString)
print(ins.MyString)

ins3 = MyInheritedClass(5)
ins3.Test()
ins3.MyString ="ins3"
print(ins3.MyString)