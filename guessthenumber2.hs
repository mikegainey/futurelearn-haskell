-- guess the number, version 2

import System.Random

guessthenumber :: IO ()
guessthenumber = do
  number <- getStdRandom (randomR (1,10))
  guessloop number

guessloop :: Int -> IO ()
guessloop number = do
  putStrLn "\nGuess the number!"
  putStr "What is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  case (compare guessNum number) of
    LT -> do putStrLn "Too low"
             guessloop number
    GT -> do putStrLn "Too high"
             guessloop number
    EQ -> putStrLn "You got it!"
