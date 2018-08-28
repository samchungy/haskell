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
initialGuess = (head gencombo, (GameState (tail gencombo)))
        where gencombo = [[d, e] | d<-allpeople, e<-allpeople, d < e]
              allpeople = [ (Person x y z) | x <- a, y <- b, z <- c ]
              a = enumFrom Short
              b = enumFrom Blond
              c = enumFrom Male

--Called to parse next guess. Extracts the best guess from groupScores function
--and returns it and the newly pruned guess list.              
nextGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> 
        ([Person], GameState)
nextGuess (ps, gs) score = (bestguess, (GameState $ delete bestguess pps))
                where bestguess = snd $ head $ head $ groupScores (ps, pruned)
                      pruned = pruneGuess (ps, gs) score
                      (GameState pps) = pruned
                      
-- Finds the frequency of the remaining pairs feedback score and sorts them
-- from lowest to highest frequency.
groupScores :: ([Person], GameState) -> [[((Int, Int, Int, Int),[Person])]]
groupScores (ps, (GameState xs)) = sortBy scmp $ groupBy cmp $ sort
                $ zip (map f xs) xs
        where f = feedback ps
              cmp = (\a b -> fst a == fst b)
              scmp = (\a b -> compare (length a) (length b)) 

-- Removes any answers which are no longer consistent with the feedback score
-- received in the last nextGuess call.              
pruneGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> GameState
pruneGuess (pp, (GameState ps)) score
        = (GameState [ x | x <- ps, score == feedback pp x])

-- Next Guess Functions End
----------------------------------------