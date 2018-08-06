-- https://wiki.haskell.org/99_questions/1_to_10

-- q1

myLast:: [a] -> a
myLast [] = error "Empty List Given"
myLast [x] = x
myLast (_:xs) = myLast xs

-- q2

myButLast:: [a] -> a
myButLast [] = error "Empty List Given"
myButLast [x] = error "The list is only 1 element long"
myButLast (x:xs)
        | length xs == 1        = x
        | otherwise             = myButLast xs
        
myButLast2:: [a] -> a
myButLast2 = last . init

-- q4

myLength:: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- q5
myReverse:: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- q6
isPalindrome:: Eq a => [a] -> Bool
isPalindrome [x] = False
isPalindrome (x:xs)
        | length (x:xs) `mod` 2 == 1            = False
        | length (x:xs) == 2 && x == last(xs)   = True
        | x == last(xs)                         = isPalindrome (init xs)
        | otherwise                             = False
        
isPalindrome2:: Eq a => [a] -> Bool
isPalindrome2 x = x == reverse x

-- q7
data NestedList a = Elem a | List [NestedList a]

-- q8

compress:: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
        | x == head xs      = compress xs
        | otherwise         = [x] ++ compress xs