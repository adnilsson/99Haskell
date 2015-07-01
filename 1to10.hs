
-- 1. (*) Find the last element of a list
-----
myLast::(Show a) => [a] -> a
myLast []        = error "An empty list has no end"
myLast (x:[])    = x
myLast (x:xs)    = myLast xs

-- 2. (*) Find the second to last element
-----
myButLast:: [a] -> a
myButLast []        = error "List is empty"
myButLast [x]       = error "List's too short"
myButLast [x,_]     = x
myButLast (x:xs)    = myButLast xs

-- 3. (*) Find the K'th element of a list, indexed from 1
-----
--elementAt:: [a] b -> a
--elementAt 

