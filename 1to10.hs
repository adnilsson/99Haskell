
-- 1. (*) Find the last element of a list
-----
myLast::(Show a) => [a] -> a
myLast  = foldl1 (\_ x -> x)

-- 2. (*) Find the second to last element
-----
myButLast:: [a] -> a
myButLast []        = error "List is empty"
myButLast [x]       = error "List's too short"
myButLast [x,_]     = x
myButLast (x:xs)    = myButLast xs

-- 3. (*) Find the K'th element of a list, indexed from 1
-----
elementAt:: [a] -> Int -> a
elementAt xs n  = head .  drop (n-1) $ xs

-- 4. (*) Find the number of elements in a list
-----
myLength:: [a] -> Int
myLength = foldl (\acc _ -> acc+1) 0

-- 5. (*) Reverse a list
-----
myReverse:: [a] -> [a]
myReverse = foldl(\acc x -> x:acc) []

-- 6. (*) Determine if a list is a palindrome
-----
isPalindrome:: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 7. (*) Flatten a nested list structure
-----
data NestedList a = Elem a | List [NestedList a]

flatten:: NestedList a -> [a] 
flatten (Elem x)        = [x]
flatten (List [])       = []
flatten (List (x:xs))   = 
