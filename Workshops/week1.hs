hello = "hello world"

xor:: Int -> Int -> Int
xor 0 0 = 0
xor 1 1 = 0
xor 1 0 = 1
xor 0 1 = 1

appen:: [a] -> [a] -> [a]
appen [] item = item
appen (x:xs) list = x:appen xs list

test:: [a] -> [a] -> [a]
test (x:xs) y = x:y

revers:: [a] -> [a]
revers [] = []
revers (x:xs) = (revers xs) ++ [x]

getNthElem:: Int -> [a] -> a
getNthElem n [] = error "List is empty"
getNthElem n (x:xs)
        | n <= 0            = error "Enter a valid number"
        | n > length (x:xs) = error "Error number bigger than list"
        | n == 1            = x
        | otherwise         = getNthElem (n-1) xs
        
getNthElem2:: Int -> [a] -> a
getNthElem 1 [x] = x
getNthElem n 