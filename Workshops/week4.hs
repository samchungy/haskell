data Tree a = Empty | Node (Tree a) a (Tree a)
list_to_bst :: Ord a => [a] -> Tree a
list_to_bst [] = Empty
list_to_bst [x] = 
list_to_bst x:xs = Node 