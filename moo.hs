-- Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows
-- to start:  *Main> moo 9305  -- where 9305 is the secret number to guess
-- The secret number should be a 4 digit number.  The digits must all be different.
-- A bull is a correct digit in the correct position.
-- A cow is a correct digit in the wrong position.

moo :: Int -> IO ()
-- given the secret number, call the game loop with counter = 1
moo secretNum = mooWithCounter secretNum 1

mooWithCounter :: Int -> Int -> IO ()
-- given the secret number and starting count, this is the game loop
mooWithCounter secretNum counter = do
  putStr "\nWhat is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  let (bulls, cows) = pureMoo secretNum guessNum
  if bulls == 4
    then putStrLn $ "You won in " ++ (show counter) ++ " moves!\n"
    else do putStrLn $ show bulls ++ " bulls, and " ++ show cows ++ " cows"
            mooWithCounter secretNum (counter + 1)

pureMoo :: Int -> Int -> (Int, Int)
-- given the secret number and the guess, returns a tuple: (bulls, cows)
pureMoo secretNum guessNum = (bulls secret guess, cows secret guess)
  where secret = num2list secretNum
        guess  = num2list guessNum

cattle :: [Int] -> [Int] -> Int
-- cattle = the number of all matches (cows + bulls)
cattle [] guess = 0
cattle secret guess = length (filter (== head secret) guess)
                      + cattle (tail secret) guess

bulls :: [Int] -> [Int] -> Int
-- the number of matches in the correct position
bulls secret guess = length $ [(s,g) | (s,g) <- zip secret guess, s == g]

cows :: [Int] -> [Int] -> Int
-- the number of cows = cattle - bulls
cows secret guess = (cattle secret guess) - (bulls secret guess)

num2list :: Int -> [Int]
-- takes a multi-digit number and returns the corresponding list of digits
-- example: 1234 -> [1,2,3,4]
num2list n
  | n < 10 = [n]
  | otherwise = num2list (n `div` 10) ++ [(n `mod` 10)]
