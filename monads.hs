-- Try to understand monads

import Control.Monad

-- instance Monad Maybe where
--   return         = Just
--   Nothing >>= f  = Nothing
--   (Just x) >>= f = f x
--   fail _         = Nothing

-- instance MonadPlus Maybe where
--   mzero             = Nothing
--   Nothing `mplus` x = x
--   x `mplus` _       = x

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead xs = Just (head xs)

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail xs = Just (tail xs)

