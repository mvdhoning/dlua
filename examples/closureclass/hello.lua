print("hello world, from Lua!\n")

print("closure test")

--http://lua-users.org/wiki/ObjectOrientationTutorial
--idea: generate below code from pascal with cfunction calls to pascal
--problem: get a pointer to the object in the self table (and hide that) think i use __hidden for that filled on create and passed along each call to pascal

local function MyClass(init)
  -- the new instance
  local self = {
    -- public fields go in the instance table
    public_field = 0
  }
  
  local __hidden = {
    --hidden table
	__test = 0
  }

  -- private fields are implemented using locals
  -- they are faster than table access, and are truly private, so the code that uses your class can't get them
  local private_field = init

  function self.foo()
    return self.public_field + private_field
  end

  function self.bar()
    private_field = private_field + 1
  end

  print("hello from myclass constructor")
  -- return the instance
  return self
end

print("create myclass")
local i = MyClass(5)
print(i.foo()) --> 5
i.public_field = 3
--i.__hidden__test = 2 --> error on private table
--print(i.__hidden.__test) --> error on private table
i.bar()
print(i.foo()) --> 9