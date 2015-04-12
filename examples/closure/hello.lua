
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