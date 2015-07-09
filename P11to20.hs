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
