-- Shuffle a list: [a] -> [a]
-- This doesn't work.  I want a function that takes a list and returns a list.
-- My hypothesis for the problem:
-- In the base case (length == 1), the output is a pure list.
-- Otherwise the random number causes the output to be IO [Int]
-- Is there a way to get the base case to return IO [Int] ?

import System.Random

shuffle1 :: [Int] -> [Int]
shuffle1 xs = do
  index <- getStdRandom (randomR (0, (length xs) - 1))
  if length xs == 1
    then xs
    else xs!!index : shuffle1 (tail xs)

shuffle2 xs
  | length xs == 1 = xs
  | otherwise = xs!!index : shuffle2 (tail xs)
  where do index <- getStdRandom (randomR (0, (length xs) - 1))
