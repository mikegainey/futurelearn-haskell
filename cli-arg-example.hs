import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
    then do
        putStrLn "usage: starman WORD NUM_GUESSES"
    else 
        let word = args!!0
            guesses = read (args!!1) :: Int
        in do
            starman word guesses

