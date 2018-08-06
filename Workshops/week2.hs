factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1
factorial n = n * factorial(n-1)

myElem :: Eq a => a -> [a] -> Bool
myElem y [] = False
myElem y (x:xs)
        | y == y        = True
        | otherwise     = myElem y xs
        
longestPrefix :: Eq a => [a] -> [a] -> [a]
longestPrefix [] y = []
longestPrefix x [] = []
longestPrefix (x:xs) (y:ys)
        | x == y        = x:longestPrefix xs ys
        | otherwise     = []
        
mccarthy_91 :: Int -> Int -> Int 
mccarthy_91 
        | n > 100       = 