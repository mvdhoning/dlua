require "TMyClass"

print("hello world, from Lua!\n")

function MyInheritedClass(init)
  --private
  local self = MyClass(init)
  
  function self.Test()
	print("Greetings from inherited class");
  end
  
  return self
end

print("MyClass Tests")
ins = MyClass()

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