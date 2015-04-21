
print("hello world, from Lua!\n")



p = {}
p.naam = 'ok'
print(p.naam)

obj = tMyObject:new() --create a new object
tMyObject.test2(obj,"hoi") --calling an method on the class
--print(obj.lees)
obj:test("hello") --this works
obj.test("hi") --no object is passed

obj:settext("test text for object1")
obj:showtext()

--obj3 = obj:new()


print("obj2 tests")

obj2 = tMyObject:new()
print("obj2 tests begin")
obj2:settext("test text for object2")
obj2:showtext()
obj:showtext()
print("obj2 tests end")

print(obj2.lees)

print(obj.lees)

print("set property test")
obj.lees = "hallo pascal, dit is lua"
obj2.lees = "object 2"
print("end set property test")

print(obj2.lees)
print(obj.lees)