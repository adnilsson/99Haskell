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
