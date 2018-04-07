-- starman.hs copied from the course example (because I couldn't do it on my own)
-- https://github.com/wimvanderbauwhede/HaskellMOOC/blob/master/Starman/starman.hs
-- I changed the order of function definitions, variable names, comments, and formatting.

starman :: String -> Int -> IO ()
starman secretWord stars = turn secretWord ['-' | x <- secretWord] stars

turn :: String -> String -> Int -> IO ()
-- check remaining stars; if secretWord and displayedWord match; else makeGuess
turn secretWord displayedWord stars = do
  if stars == 0
    then putStrLn "You Lose!"
    else if displayedWord == secretWord
           then putStrLn "You Win!"
           else makeGuess secretWord displayedWord stars

makeGuess :: String -> String -> Int -> IO ()
-- show status; get user input;
makeGuess secretWord displayedWord stars = do
  putStrLn $ displayedWord ++ "  " ++ take stars (repeat '*')
  putStr "your guess: "
  guess <- getLine
  let (correct, nextDisplayedWord) = check secretWord displayedWord (guess!!0)
  let nextStars = if correct then stars else stars - 1
  turn secretWord nextDisplayedWord nextStars

check :: String -> String -> Char -> (Bool, String)
-- after a guess, return a tuple: (Bool, nextDisplayedWord)
check secretWord displayedWord guess =
     (guess `elem` secretWord,
      [ if guess == s then s else d | (d, s) <- zip displayedWord secretWord])


