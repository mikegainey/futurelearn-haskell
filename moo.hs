-- Bulls and Cows game: https://en.wikipedia.org/wiki/Bulls_and_Cows

moo secretNum = do
  putStr "What is your guess? "
  guessStr <- getLine
  let guessNum = read guessStr :: Int
  let (bulls, cows) = pureMoo secretNum guessNum
  print (bulls, cows)
  moo secretNum


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
