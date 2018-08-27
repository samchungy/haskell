-- Sam Chung | 758 053
-- Project 1 Declarative Programming 2018 Semester 2

--module Proj1 (Person, parsePerson, height, hair, sex,
  --     GameState, initialGuess, nextGuess, feedback) where

import Data.List 
  
data Height = Short | Tall
        deriving (Show, Eq, Enum, Ord)
data Hair = Blond | Red | Dark
        deriving (Show, Eq, Enum, Ord)
data Sex = Male | Female
        deriving (Show, Eq, Enum, Ord)
data Person = Person Height Hair Sex
        deriving (Show, Eq, Ord)

-- Stores the state
-- [[Person]] - Holds remaining pair lineups
data GameState = GameState [[Person]]
        deriving (Show, Eq)

height:: Person -> Height
height (Person a b c) = a

hair :: Person -> Hair
hair (Person a b c) = b

sex :: Person -> Sex
sex (Person a b c) = c

------------------------------------------
-- ParsePerson Functions

parsePerson :: String -> Maybe Person
parsePerson x
        | lenx == 3 && valheight && valhair && valsex = Just
                (Person (newHeight xheight) (newHair xhair) (newSex xsex))
        | otherwise = Nothing
        where lenx = length x
              xheight = head x
              xhair = x !! 1
              xsex = last x
              valheight = elem xheight ['S','T']
              valhair = elem xhair ['B','R','D']
              valsex = elem xsex ['M','F']

newHeight :: Char -> Height
newHeight x
        | x == 'S'      = Short
        | x == 'T'      = Tall
        | otherwise     = error "Invalid Height"
        
newHair :: Char -> Hair
newHair x
        | x == 'B'      = Blond
        | x == 'R'      = Red
        | x == 'D'      = Dark
        | otherwise     = error "Invalid Hair Colour"

newSex :: Char -> Sex
newSex x
        | x == 'M'      = Male
        | x == 'F'      = Female
        | otherwise     = error "Invalid Sex"

-- End Parse Functions
----------------------------------------
        
----------------------------------------
-- Feedback Functions
feedback :: [Person] -> [Person] -> (Int, Int, Int, Int)
feedback [c1, c2] [s1, s2] = (
        eqtest (sort [c1, c2]) (sort [s1, s2]),
        eqtest (sort [(height c1), (height c2)]) 
                (sort [(height s1), (height s2)]),
        eqtest (sort [(hair c1), (hair c2)]) (sort [(hair s1), (hair s2)]),
        eqtest (sort [(sex c1), (sex c2)]) (sort [(sex s1), (sex s2)])
        )

-- Checks for one attribute matching the pair.
eqtest :: Ord a => [a] -> [a] -> Int
eqtest [] [] = 0
eqtest (x:xs) (y:ys)
        | x == y      = 1 + eqtest xs ys
        | otherwise   = 0 + eqtest xs ys
               
-- Feedback Functions End
----------------------------------------        
        
initialGuess:: ([Person],GameState)
initialGuess = (head gencombo, (GameState (tail gencombo)))
        where gencombo = [[d, e] | d<-allpeople, e<-allpeople, d <= e]
              allpeople = [ (Person x y z) | x <- a, y <- b, z <- c ]
              a = enumFrom Short
              b = enumFrom Blond
              c = enumFrom Male

----------------------------------------
-- Next Guess Functions
              
--nextGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> 
  --      ([Person], GameState)
        
pruneGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> GameState
pruneGuess (pp, (GameState ps)) (w, x, y, z)
        = (GameState [ i | i <- ps, let (a, b, c, d) = feedback pp i,
                x == b && y == c && z == d])

-- Next Guess Functions End
----------------------------------------