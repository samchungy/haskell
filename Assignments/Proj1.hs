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
feedback [c1, c2] [s1, s2] = 
        (
                (eqtest [c1, c2] [s1, s2]),
                (eqtest  [height c1, height c2] [height s1, height s2]),
                (eqtest [hair c1, hair c2] [hair s1, hair s2]),
                (eqtest [sex c1, sex c2] [sex s1, sex s2])
        )

--Removes from 1 list the elements of the other list. Returns the number of
--elements which match        
eqtest :: Eq a => [a] -> [a] -> Int
eqtest l1 l2
        | lendups == 0       = 2
        | lendups == 1       = 1
        | otherwise          = 0
        where lendups = length (l1 \\ l2)
               
-- Feedback Functions End
----------------------------------------        
        
initialGuess :: ([Person],GameState)
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

frequency :: ([Person], GameState) -> [((Int, Int, Int, Int),[Person])]
frequency (ps, (GameState xs)) = zip (map f xs) xs
        where f = feedback ps
        
pruneGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> GameState
pruneGuess (pp, (GameState ps)) score
        = (GameState [ i | i <- ps, score == feedback pp i])

-- Next Guess Functions End
----------------------------------------