
print("hello world, from Lua!\n")

p = {}
p.naam = 'ok'
print(p.naam)

obj = tMyObject:new() --create a new object
tMyObject.test2(obj,"hoi") --calling an method on the class
obj:test2("hello") --this works
obj.test("hi") --no object is passed