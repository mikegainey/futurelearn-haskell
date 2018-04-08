-- Shuffle a list.

import System.Random

shuffle xs = do
  let lenxs = length xs
  index <- getStdRandom (randomR (0, lenxs - 1))
  if length xs == 1
    then xs
    else xs!!index : shuffle (tail xs)
