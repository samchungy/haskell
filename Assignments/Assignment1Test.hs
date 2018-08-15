--  File     : publictests.hs
--  Author   : Peter Schachte
--  Purpose  : test cases for Assignment1 project

import Assignment1
import HaskellTest

suite = 
  TimeLimit 2.0 $
  Suite [
    expect (elementPosition 3 [1,2,3,4,5]) (2),
    expect (everyNth 4 "elephant") ("pt"),
    expect (sumEarlier [1,2,3,4,5]) [1,3,6,10,15],
    expect (sumLater [1,2,3,4,5]) [15,14,12,9,5]
    ]

main :: IO ()
main = do
  testVerbose suite
