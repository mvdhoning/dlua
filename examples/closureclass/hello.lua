print("hello world, from Lua!\n")

print("closure test")

--http://lua-users.org/wiki/ObjectOrientationTutorial
--http://www.computercraft.info/forums2/index.php?/topic/15013-setget-members-in-an-object-oriented-class/ (for properties)
--idea: generate below code from pascal with cfunction calls to pascal
--problem: get a pointer to the object in the self table (and hide that) think i use __hidden for that filled on create and passed along each call to pascal

local function BaseClass()
  
  local _members = { }

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
        end
  })

end

-- this will be the class generated from pascal
function MyClass(init)
  --private
  local self = BaseClass()
  local _privateVariable = init
  local function _privateVariableSetter(v)
        _privateVariable = 2 * v
  end
  print("hello from MyClass constructor")
  --public 
  --functions
  function self.foo()
    return "hello!"
  end
  --properties
  self._member = { key="var",
                                        set=_privateVariableSetter,
                                        get=function() return _privateVariable end }
  return self
end
--end generated from pascal

-- start of actual lua script
print("create myclass")
instance = BaseClass()
instance._member = {key="type", get=function() return "class" end}
print(instance.type) --> class

print("inherited")
ins = MyClass(5)
print(ins.foo()) --> Hello!
print(ins.var) --> 5
ins.var = 2
print(ins.var) --> 4