-- Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows
-- to start:  *Main> moo 9305  -- where 9305 is the secret number to guess
-- The secret number should be a 4 digit number
-- A bull is a correct digit in the correct position
-- A cow is a correct digit in the wrong position

-- given the secret number, calls the game loop with counter = 1
moo :: Int -> IO ()
moo secretNum = mooWithCounter secretNum 1

-- given the secret number and starting count, this is the game loop
mooWithCounter :: Int -> Int -> IO ()
mooWithCounter secretNum counter = do
  putStr "\nWhat is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  let (bulls, cows) = pureMoo secretNum guessNum
  if bulls == 4
    then putStrLn $ "You won in " ++ (show counter) ++ " moves!\n"
    else do putStrLn $ show bulls ++ " bulls, and " ++ show cows ++ " cows"
            mooWithCounter secretNum (counter + 1)

-- given the secret number and the guess, returns a tuple: (bulls, cows)
pureMoo :: Int -> Int -> (Int, Int)
pureMoo secretNum guessNum = (bulls secret guess, cows secret guess)
  where secret = num2list secretNum
        guess  = num2list guessNum

-- cattle = the number of all matches (cows + bulls)
cattle :: [Int] -> [Int] -> Int
cattle [] guess = 0
cattle secret guess = length (filter (== head secret) guess)
                      + cattle (tail secret) guess

-- the number of matches in the correct position
bulls :: [Int] -> [Int] -> Int
bulls secret guess = length $ [(s,g) | (s,g) <- zip secret guess, s == g]

-- the number of cows = cattle - bulls
cows :: [Int] -> [Int] -> Int
cows secret guess = (cattle secret guess) - (bulls secret guess)

-- takes a multi-digit number and produces the corresponding list of digits
-- example: 1234 -> [1,2,3,4]
num2list :: Int -> [Int]
num2list n
  | n < 10 = [n]
  | otherwise = num2list (n `div` 10) ++ [(n `mod` 10)]
