-- Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows
-- to start:  *Main> moo 9305  -- where 9305 is the secret number to guess
-- The secret number should be a 4 digit number.  The digits must all be different.
-- A bull is a correct digit in the correct position.
-- A cow is a correct digit in the wrong position.

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
cows secret guess = (cattleVer3 secret guess) - (bulls secret guess)

cattleVer1 :: [Int] -> [Int] -> Int
-- cattleVer1 = the number of all matches (cows + bulls)
cattleVer1 [] guess = 0
cattleVer1 secret guess = length (filter (== head secret) guess)
                      + cattleVer1 (tail secret) guess

cattleVer2 :: [Int] -> [Int] -> Int
-- cattleVer2 = the number of all matches (cows + bulls)
cattleVer2 secret guess = sum $ map (\s -> length $ filter (==s) guess) secret

cattleVer3 :: [Int] -> [Int] -> Int
-- cattleVer3 = the number of all matches (cows + bulls)
cattleVer3 secret guess = length $ [ 1 | s <- secret, g <- guess, s == g]

num2list :: Int -> [Int]
-- take a multi-digit number and return the corresponding list of digits
-- example: 1234 --> [1,2,3,4]
num2list n
  | n < 10 = [n]
  | otherwise = num2list (n `div` 10) ++ [(n `mod` 10)]
