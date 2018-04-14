-- Shuffle a list: [a] -> [a]
-- This doesn't work.  I want a function that takes a list and returns a list
-- made by shuffling the input list.
-- example: [1,2,3,4,5] --> [3,1,4,5,2]
-- Apparently, this is a difficult problem in Haskell.  Here's how to do it:
-- https://wiki.haskell.org/Random_shuffle

import System.Random

shuffle :: [Int] -> [Int]
shuffle xs = do
  index <- getStdRandom (randomR (0 :: Int, (length xs) - 1))
  if length xs == 1
    then return xs
    else return $ xs!!index : shuffle (tail xs)
