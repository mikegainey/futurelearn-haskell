-- Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows
-- to start:  *Main> moo 9305  -- where 9305 is the secret number to guess
-- The secret number should be a 4 digit number.  The digits must all be different.
-- A bull is a correct digit in the correct position.
-- A cow is a correct digit in the wrong position.

import System.Random
import Data.Array.IO
import Control.Monad

moo :: Int -> IO ()
-- given the secret number, call the game loop with counter = 1
moo secretNum = mooWithCounter secretNum 1

mooWithCounter :: Int -> Int -> IO ()
-- given the secret number and starting count; this is the game loop
mooWithCounter secretNum counter = do
  putStr "\nWhat is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  let (bulls, cows) = pureMoo secretNum guessNum
  if bulls == 4
    then putStrLn $ "You won in " ++ (show counter) ++ " moves!\n"
    else do putStrLn $ (show bulls) ++ " bulls, and " ++ (show cows) ++ " cows"
            mooWithCounter secretNum (counter + 1)

pureMoo :: Int -> Int -> (Int, Int)
-- given the secret number and the guess, return a tuple: (bulls, cows)
pureMoo secretNum guessNum = (bulls secret guess, cows secret guess)
  where secret = num2list secretNum
        guess  = num2list guessNum

bulls :: [Int] -> [Int] -> Int
-- the number of matches in the correct position
bulls secret guess = length $ [(s,g) | (s,g) <- zip secret guess, s == g]

cows :: [Int] -> [Int] -> Int
-- the number of cows = cattle - bulls
cows secret guess = (cattle secret guess) - (bulls secret guess)

cattle :: [Int] -> [Int] -> Int
-- cattle = the number of all matches (cows + bulls)
cattle secret guess = length $ [ (s,g) | s <- secret, g <- guess, s == g]

num2list :: Int -> [Int]
-- take a multi-digit number and return the corresponding list of digits
-- example: 1234 --> [1,2,3,4]
num2list n
  | n < 10 = [n]
  | otherwise = num2list (n `div` 10) ++ [(n `mod` 10)]

str2list :: String -> [Int]
-- given a "number string", return a list of digits: "0123" --> [0,1,2,3]
str2list str = map (\d -> read [d] :: Int) str

--------------------------------------------------------------------------------
-- This will copy/pasted from https://wiki.haskell.org/Random_shuffle because I
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

--------------------------------------------------------------------------------

-- Notes
-- 1. Bug: Start the program with moo 9502
--         Guessing 0295 results in 0 bulls, and 3 cows (instead of 0 bulls & 4 cows).
--         The problem is in num2list: num2list 0123 --> [1,2,3] instead of [0,1,2,3]
-- 2. pureMoo calls both the bulls and the cows function; and the cows function calls the bulls function
--    I could refactor so bulls is only called once, but at the expense of readability.
