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

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort lesser ++ [x] ++  quickSort greater
        where
                lesser = filter (< x) xs
                greater = filter (>= x) xs

data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)
        deriving (Eq, Show)

same_shape :: Tree a b -> Tree c d -> Bool
same_shape Leaf (Node _ _ _ _) = False
same_shape (Node _ _ _ _) Leaf = False
same_shape Leaf Leaf = True
same_shape (Node k1 v1 l1 r1) (Node k2 v2 l2 r2) = same_shape l1 l2 && same_shape r1 r2
