require "MyClass"

print("hello world, from Lua!\n")



function MyInheritedClass(init)
  --private
  local self = TMyClass(init)
  
  function self.Test()
	print("Greetings from inherited class");
  end

  return self
end

print("MyClass Tests")
ins = TMyClass()
ins.MyVar = "test"
print(ins.Show()) --> Show: test 

ins.MyString = "Hoi"
print(ins.MyString)

ins2 = TMyClass()
ins2.MyString = "Hallo"
print(ins2.MyString)
print(ins.MyString)

ins3 = MyInheritedClass(5)
ins3.Show()
ins3.Test()
ins3.MyString ="ins3"
print(ins3.MyString)

ins3.Merge("hi")

--object set in pascal
test.Show()
test.MyString = "text from lua"

mytest.Show()
