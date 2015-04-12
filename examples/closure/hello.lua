
print("hello world, from Lua!\n")

c1 = newCounter()
print(c1())  --> 1
print(c1())  --> 2
print(c1())  --> 3

c2 = newCounter()
print(c2())  --> 1
print(c2())  --> 2

print(c1())  --> 4

c3 = test:new()
print(c3.get()) --> 1 
print(c3.get()) --> 2

print(c3.j) --> 5

print("test1")

c3.j = 3.0
print(c3.j) --> 3

c3.x = 2.0  -- add a fake property
print(c3.x) --> 2

print("test3")
c3.j = 4.0
print(c3.set()) --> 4
c4 = test:new()
c4.j = 6.0
print(c4.set()) --> 6
print(c3.set()) --> 4