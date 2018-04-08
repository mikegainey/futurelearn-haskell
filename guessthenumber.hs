-- guess the number

guessthenumber :: Int -> IO ()
guessthenumber number = do
  putStrLn "\nGuess the number!"
  putStr "What is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  case (compare guessNum number) of
    LT -> do putStrLn "Too low"
             guessthenumber number
    GT -> do putStrLn "Too high"
             guessthenumber number
    EQ -> putStrLn "You got it!"
