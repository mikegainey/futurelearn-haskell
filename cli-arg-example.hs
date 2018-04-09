-- from https://gist.github.com/jeremysinger/c77de1342921e0834769b050a21527e1

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

-- Then you can do
-- $ ghc starman.hs
-- $ ./starman hello 3
-- or
-- $ runhaskell starman hello 3
