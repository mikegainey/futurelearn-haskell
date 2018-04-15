-- Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows
-- to start:  *Main> moo

import System.Random
import Data.Array.IO
import Control.Monad

moo :: IO ()
-- given the secret number string, call the game loop with counter = 1
moo = do
  putStrLn "\nThis is the Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows"
  putStrLn "Guess the secret 4-digit number (each digit appears only once)."
  putStrLn "A bull is a correct digit in the correct position."
  putStrLn "A cow is a correct digit in the wrong position."
  secret <- fmap (take 4) (shuffle [0..9])
  -- print secret  -- the secret number
  mooWithCounter secret 1

mooWithCounter :: [Int] -> Int -> IO ()
-- given the secret number string and starting count; this is the game loop
mooWithCounter secret counter = do
  putStr "\nWhat is your guess? "
  guessStr <- getLine
  let guess = str2list guessStr
  let (bulls, cows) = pureMoo secret guess
  if bulls == 4
    then putStrLn $ "You won in " ++ (show counter) ++ " moves!\n"
    else do putStrLn $ (show bulls) ++ " bulls, and " ++ (show cows) ++ " cows"
            mooWithCounter secret (counter + 1)

pureMoo ::[Int] -> [Int] -> (Int, Int)
-- given the secret number and the guess, return a tuple: (bulls, cows)
pureMoo secret guess = (bulls secret guess, cows secret guess)

bulls :: [Int] -> [Int] -> Int
-- the number of matches in the correct position
bulls secret guess = length $ [(s,g) | (s,g) <- zip secret guess, s == g]

cows :: [Int] -> [Int] -> Int
-- the number of cows = cattle - bulls
cows secret guess = (cattle secret guess) - (bulls secret guess)

cattle :: [Int] -> [Int] -> Int
-- cattle = the number of all matches (cows + bulls)
cattle secret guess = length $ [ (s,g) | s <- secret, g <- guess, s == g]

str2list :: String -> [Int]
-- given a "number string", return a list of digits: "0123" --> [0,1,2,3]
str2list str = map (\d -> read [d] :: Int) str

--------------------------------------------------------------------------------
-- This was copied from https://wiki.haskell.org/Random_shuffle because I
-- couldn't figure out how to do it myself.

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
