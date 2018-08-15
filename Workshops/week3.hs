ftoc :: Double -> Double
ftoc x = (5/9) * (x-32)


quadRoots :: Double -> Double -> Double -> [Double]
quadRoots a b c = [(-b + sqrt (b*b - 4*a*c))/(2*a), (-b - sqrt (b*b - 4*a*c))/(2*a)]

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists x [] = x
mergeSortedLists [] x = x
mergeSortedLists (x:xs) (y:ys)
    | x <= y            = (x:mergeSortedLists xs (y:ys))
    | otherwise         = (y:mergeSortedLists (x:xs) ys)