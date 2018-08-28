-- Sam Chung | 758 053
-- Project 1 Declarative Programming 2018 Semester 2
-- Objective: To implement the guessing component of the Robbery Culprit Game

module Proj1 (Person, parsePerson, height, hair, sex,
        GameState, initialGuess, nextGuess, feedback) where

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
type GameState = [[Person]]

height:: Person -> Height
height (Person a b c) = a

hair :: Person -> Hair
hair (Person a b c) = b

sex :: Person -> Sex
sex (Person a b c) = c

------------------------------------------
-- ParsePerson Functions

-- Validates, then parses the string to a type.
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

-- Takes a pair and compares it with the other pair.        
feedback :: [Person] -> [Person] -> (Int, Int, Int, Int)
feedback l1 l2 =
        (
                (oldlen - newlen),
                (eqtest (map height newl1) (map height newl2)),
                (eqtest (map hair newl1) (map hair newl2)),
                (eqtest (map sex newl1) (map sex newl2))
        )
        where newlen = length newl1
              oldlen = length l1
              newl1 = l1 \\ l2
              newl2 = l2 \\ l1

--Removes from 1 list the elements of the other list. Returns the number of
--elements which match.       
eqtest :: Eq a => [a] -> [a] -> Int
eqtest l1 l2 = oldlen - newlen
        where newlen = length (l1 \\ l2)
              oldlen = length l1
               
-- Feedback Functions End
---------------------------------------- 

----------------------------------------
-- Guess Functions

--First called before nextGuess        
initialGuess :: ([Person],GameState)
initialGuess = (ing, (delete ing gencombo))
        where gencombo = [[d, e] | d<-allpeople, e<-allpeople, d < e]
              allpeople = [ (Person x y z) | x <- a, y <- b, z <- c ]
              a = enumFrom Short
              b = enumFrom Blond
              c = enumFrom Male
              ing = [Person Short Blond Male, Person Short Red Male]

--Called to parse next guess. Uses calcAverage and genLineupScores to calculate
-- the best guess
nextGuess (ps, gs) score = (bestguess, (delete bestguess prune))
            where bestguess = snd $ head sortedavg
                  sortedavg = sort avgexpect
                  avgexpect = zip (calcAverage (genLineupScores prune)) prune
                  prune = pruneGuess (ps, gs) score

--Calculates the average number of expected guesses per guess               
calcAverage :: [[Int]] -> [Double]
calcAverage (x:xs) = map sum (map (map (f sumx)) (x:xs))
        where sumx = sum x
              f = (\a b -> fromIntegral b * (fromIntegral b)/(fromIntegral a))

--Generates the feedback scores between each of the guesses
genLineupScores :: GameState -> [[Int]]
genLineupScores gs = [ map length (group $ sort $ map f gs) | 
        x <- gs, let f = feedback x]
        
-- Removes any answers which are no longer consistent with the feedback score
-- received in the last nextGuess call.   
pruneGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> GameState
pruneGuess (pp, ps) score
        = [ i | i <- ps, score == feedback pp i]

-- Next Guess Functions End
----------------------------------------