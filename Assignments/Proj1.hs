-- Sam Chung | 750 053
-- Project 1 Declarative Programming 2018 Semester 2

--module Proj1 (Person, parsePerson, height, hair, sex,
--       GameState, initialGuess, nextGuess, feedback) where

data Height = Short | Tall
        deriving (Show, Eq, Enum)
data Hair = Blond | Red | Dark
        deriving (Show, Eq, Enum)
data Sex = Male | Female
        deriving (Show, Eq, Enum)
data Person = Person Height Hair Sex
        deriving (Show, Eq)

-- Stores the state
-- [[Person]] - Holds remaining suspects  
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

-- Accumulates using a,,b,,c,d variables
feedback :: [Person] -> [Person] -> (Int, Int, Int, Int)
feedback [] _ = (0, 0, 0, 0)
feedback ((Person x1 x2 x3):xs) [(Person y1 y2 y3),(Person y21 y22 y23)]
        = let (a,b,c,d) = feedback xs [p2,p3] in 
                (
                        (a + (eqtest p1 p2 p3)),
                        (b + (eqtest x1 y1 y21)),
                        (c + (eqtest x2 y2 y22)),
                        (d + (eqtest x3 y3 y23))
                )
        where p1 = (Person x1 x2 x3)
              p2 = (Person y1 y2 y3)
              p3 = (Person y21 y22 y23)

-- Checks for one attribute matching the pair.
eqtest :: Eq a => a -> a -> a -> Int
eqtest x y z
        | x == y || x == z       = 1
        | otherwise              = 0
        
-- Feedback Functions End
----------------------------------------        
        
initialGuess:: ([Person],GameState)
initialGuess = (initg, (GameState (tail gencombo)))
        where initg = head gencombo
              gencombo = [[d, e] | d<-allpeople, e<-allpeople]
              allpeople = [ (Person x y z) | x <- a, y <- b, z <- c ]
              a = enumFrom Short
              b = enumFrom Blond
              c = enumFrom Male
--nextGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> ([Person], GameState)
