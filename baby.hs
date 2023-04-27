-- ghci :l baby.hs
-- :r to reload

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

-- pattern matching
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x 

-- guards
-- using guards instead of patterns for a boolean condition.
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

-- where
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

-- let <bindings> in <expression>
-- let bindings are expressions themselves. where bindings are just syntactic constructs.
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

-- case expressions
-- case expression of pattern -> result
-- can convert to normal pattern matching

-- case expression of pattern -> result  
--                    pattern -> result  
--                    pattern -> result  
--                    ...  



-- recursion

-- recursive function
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) 

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs  

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum'' xs)  

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

repeat' :: a -> [a]  
repeat' x = x:repeat' x  

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs  

-- quicksort
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted 

-- higher order functions
-- functions that take functions as parameters and/or return functions as return values
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  

-- It takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  
-- ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]  
-- [6,8,7,9]  
-- ghci> zipWith' max [6,3,2,1] [7,3,1,5]  
-- [7,3,2,5]  
-- ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]  
-- ["foo fighters","bar hoppers","baz aldrin"]  
-- ghci> zipWith' (*) (replicate 5 2) [1..]  
-- [2,4,6,8,10]  
-- ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
-- [[3,4,6],[9,20,30],[10,12,12]] 

-- ghci> flip' zip [1,2,3,4,5] "hello"  
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]  
-- ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]  
-- [5,4,3,2,1]  

-- Maps and filters
-- map :: (a -> b) -> [a] -> [b]  
-- map _ [] = []  
-- map f (x:xs) = f x : map f xs  

-- filter :: (a -> Bool) -> [a] -> [a]  
-- filter _ [] = []  
-- filter p (x:xs)   
--     | p x       = x : filter p xs  
--     | otherwise = filter p xs 

-- ghci> let listOfFuns = map (*) [0..]  
-- ghci> (listOfFuns !! 4) 5  
-- 20  

-- lambdas
-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5] 
-- [153.0,61.5,31.0,15.75,6.6] 

-- folds
sum'' :: (Num a) => [a] -> a  
sum'' xs = foldl (\acc x -> acc + x) 0 xs  
-- ghci> sum'' [3,5,2,1]  
-- 11  

elem'' :: (Eq a) => a -> [a] -> Bool  
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

map' :: (a -> b) -> [a] -> [b]  
-- right fold
map' f xs = foldr (\x acc -> f x : acc) [] xs  
-- left fold, ++ function is much more expensive than :
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs


-- Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that.
-- Whenever you want to traverse a list to return something, chances are you want a fold.
-- That's why folds are, along with maps and filters, one of the most useful types of functions in functional programming.

-- Function application with $, equals to ()

-- Function composition
-- ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24] 