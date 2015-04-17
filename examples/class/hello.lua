
print("hello world, from Lua!\n")



p = {}
p.naam = 'ok'
print(p.naam)

obj = tMyObject:new() --create a new object
tMyObject.test2(obj,"hoi") --calling an method on the class
obj:test("hello") --this works
--obj.test("hi") --no object is passed

obj:settext("test text for object1")
obj:showtext()

-- Add method to instance as use it as class
function obj:doSomething()
   print("do something")
end

--obj3 = obj:new()


print("obj2 tests")

obj2 = tMyObject:new()
obj2:settext("test text for object2")
obj2:showtext()
obj:showtext()

print("lees tests")
print(obj.lees)
obj.lees = "hallo pascal"

--obj.showtext()
--obj3:doSomething()