-- functions:
doubleMe x = x + x  
doubleUs x y = x*2 + y*2   
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2   

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

-- The return type is the last item in the declaration and the parameters are the first three.
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

-- head :: [a] -> a  
-- fst :: (a, b) -> a  


-- A typeclass is a sort of interface that defines some behavior.

-- ghci> :t (==)  
-- (==) :: (Eq a) => a -> a -> Bool  

-- ghci> :t (>)  
-- (>) :: (Ord a) => a -> a -> Bool  