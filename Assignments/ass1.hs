elementPosition :: Eq t => t -> [t] -> Int
elementPosition el [] = error "Empty List"
elementPosition el (x:xs)
        | el == x           = 0
        | null xs           = 1
        | otherwise         = 1 + elementPosition el xs
    
everyNth :: Int -> [t] -> [t]
everyNth n [] = []
everyNth n list
        | n <= 0                = error "Number must be greater than zero"
        | n <= length list      = list !! (n-1) : everyNth n (drop n list)
        | otherwise             = []
        
sumLater :: Num a => [a] -> [a]
sumLater [] = []
sumLater (x:xs) = [sum (x:xs)] ++ sumLater xs

sumEarlier :: Num a => [a] -> [a]
sumEarlier [] = []
sumEarlier list = sumEarlier (init list) ++ [sum list]