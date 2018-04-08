-- Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows
-- to start:  *Main> moo 9305  -- where 9305 is the secret number to guess
-- A bull is a correct digit in the correct position
-- A cow is a correct digit in the wrong position

moo :: Int -> IO ()
moo secretNum = mooWithCounter secretNum 1

mooWithCounter :: Int -> Int -> IO ()
mooWithCounter secretNum moves = do
  putStr "\nWhat is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  let (bulls, cows) = pureMoo secretNum guessNum
  if bulls == 4
    then putStrLn $ "You won in " ++ (show moves) ++ " moves!\n"
    else do putStrLn $ show bulls ++ " bulls, and " ++ show cows ++ " cows"
            mooWithCounter secretNum (moves + 1)

pureMoo :: Int -> Int -> (Int, Int)
pureMoo secretNum guessNum = (bulls secret guess, cows secret guess)
  where secret = num2list secretNum
        guess  = num2list guessNum

cattle :: [Int] -> [Int] -> Int
cattle [] guess = 0
cattle secret guess = length (filter (== head secret) guess)
                      + cattle (tail secret) guess

bulls :: [Int] -> [Int] -> Int
bulls secret guess = length $ [(s,g) | (s,g) <- zip secret guess, s == g]

cows :: [Int] -> [Int] -> Int
cows secret guess = (cattle secret guess) - (bulls secret guess)

num2list :: Int -> [Int]
num2list n
  | n < 10 = [n]
  | otherwise = num2list (n `div` 10) ++ [(n `mod` 10)]
