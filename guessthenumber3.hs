-- guess the number, version 3.  Now keeping track of how many guesses are made.
-- to start:  *Main> guessthenumber  -- with no arguments

import System.Random

guessthenumber :: IO ()
guessthenumber = do
  number <- getStdRandom (randomR (1,10))
  guessloop number 1

guessloop :: Int -> Int -> IO ()
guessloop number guesses = do
  putStrLn "\nGuess the number!"
  putStr "What is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  case (compare guessNum number) of
    LT -> do putStrLn "Too low"
             guessloop number (guesses + 1)
    GT -> do putStrLn "Too high"
             guessloop number (guesses + 1)
    EQ -> putStrLn ("You got it in " ++ (show guesses) ++ " guesses!\n")
