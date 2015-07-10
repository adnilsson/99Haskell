import P1to10  

-- (*) 11. Modify P10 
----
data Item a = Multiple Int a | Single a deriving Show 

encodeModified:: Eq a => [a] -> [Item a] 
encodeModified xs   = map encodeModified' (encode xs)

encodeModified':: Eq a => (Int, a) -> Item a
encodeModified' (n, a) 
    | n < 2     = Single a 
    | otherwise = Multiple n a

-- (**) 12. Decode a run-length encoded list 
----
decodeModified:: [Item a] -> [a]
decodeModified  []      = []
decodeModified  (x:xs)  = decodeElem x ++ decodeModified xs

decodeElem:: Item a -> [a]
decodeElem (Single x)       = [x]
decodeElem (Multiple n x)   = replicate n x

-- (**) 13. Run-length encoding of a list directly 
----
encodeDirect:: Eq a => [a] -> [Item a]
encodeDirect [] = []
encodeDirect xs = encodeDirect' 1 xs

encodeDirect':: Eq a => Int -> [a] -> [Item a]
encodeDirect' _ []  = []
encodeDirect' n [x] = [makeElem n x]
encodeDirect' n (x1:x2:xs) 
    | x1 == x2  = encodeDirect' (n+1) (x2:xs)
    | otherwise = (makeElem n x1):(encodeDirect' 1 (x2:xs))

makeElem:: Int -> a -> Item a 
makeElem n x
    | n > 1     = Multiple n x
    | n == 1    = Single x
    | otherwise = error "n must be non-negative"

-- (*) 14. Duplicate every list element
----
dupli:: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) [] 

-- (**) 15. Replicate all list elements a given number of times
----
repli:: Int -> [a] -> [a]
repli n xs = foldr (\x acc -> (repli' n x) ++ acc) [] xs
    where repli' n x = take n $ repeat x

-- (**) 16. Drop every n-th element from a list 
---- 
dropEvery:: [a] -> Int -> [a]
dropEvery xs n  = dropEvery' xs n n

dropEvery':: [a] -> Int -> Int -> [a]
dropEvery' [] _ _       = []
dropEvery' (x:xs) 1 n  = dropEvery' xs n n
dropEvery' (x:xs) n n0  
    | n > 1     = x:dropEvery' xs (n-1) n0
    | otherwise = []

-- (*) 17. Split a list into two parts
---- It works, but I don't like it.
-----
split:: [a] -> Int -> ([a],[a])
split xs n  = head $ zip ([take' xs n]) ([drop' xs n])

drop':: [a] -> Int -> [a]
drop' xs 0  = xs
drop' [] _  = []
drop' (x:xs) n = drop' xs (n-1) 

take':: [a] -> Int -> [a]
take' xs 0  = [] 
take' [] _  = []
take' (x:xs) n = x:take' xs (n-1)
