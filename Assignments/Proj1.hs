-- Sam Chung | 758 053
-- Project 1 Declarative Programming 2018 Semester 2

module Proj1 (Person, parsePerson, height, hair, sex,
       GameState, initialGuess, nextGuess, feedback) where

data Height = Short | Tall
        deriving (Show, Eq, Enum)
data Hair = Blond | Red | Dark
        deriving (Show, Eq, Enum)
data Sex = Male | Female
        deriving (Show, Eq, Enum)
data Person = Person Height Hair Sex
        deriving (Show, Eq)

-- Stores the state
-- [[Person]] - Holds remaining pair lineups
data GameState = GameState [[Person]] [Height] [Hair] [Sex]
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
-- c1 = Culprit 1, Sus1, Sus2 = Suspect 1 & 2
feedback :: [Person] -> [Person] -> (Int, Int, Int, Int)
feedback [] _ = (0, 0, 0, 0)
feedback (c1:cs) [sus1,sus2] = let (a,b,c,d) = feedback cs [sus1,sus2] in 
                (
                        (a + (eqtest c1 sus1 sus2)),
                        (b + (eqtest (height c1) (height sus1) (height sus2))),
                        (c + (eqtest (hair c1) (hair sus1) (hair sus2))),
                        (d + (eqtest (sex c1) (sex sus1) (sex sus2)))
                )

-- Checks for one attribute matching the pair.
eqtest :: Eq a => a -> a -> a -> Int
eqtest x y z
        | x == y || x == z       = 1
        | otherwise              = 0
        
-- Feedback Functions End
----------------------------------------        
        
initialGuess:: ([Person],GameState)
initialGuess = (initg, (GameState gencombo), a, b, c)
        where initg = head gencombo
              gencombo = [[d, e] | d<-allpeople, e<-allpeople]
              allpeople = [ (Person x y z) | x <- a, y <- b, z <- c ]
              a = enumFrom Short
              b = enumFrom Blond
              c = enumFrom Male

----------------------------------------
-- Next Guess Functions
              
nextGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> 
        ([Person], GameState)
nextGuess (ps, gs) score = (bestguess, newgame)
        where newgame = pruneState ps gs score
              
pruneState :: [Person] -> GameState -> (Int, Int, Int, Int) -> 
        GameState
pruneState 

-- Next Guess Functions End
----------------------------------------