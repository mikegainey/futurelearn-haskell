-- Shuffle a list.

import System.Random

shuffle xs = do
  let lenxs = length xs
  index <- getStdRandom (randomR (0, lenxs - 1))
  print lenxs
