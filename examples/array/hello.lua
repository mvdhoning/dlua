print("hello world, from Lua!\n")
print("create test array")
testa = array.new(1000)
print("print test array size")
print (array.size(testa))
print ("fill test array")
for i=1,1000 do
  array.set(testa, i, 1/i)
end
print ("print element 10 from array")
print (array.get(testa, 10))  
print (1/10)
array.free(testa)
