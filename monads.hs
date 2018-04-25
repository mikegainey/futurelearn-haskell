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

-- a function to return the 3rd element of a list (if it exists), else Nothing
-- this uses explicit Maybe
third1 :: [a] -> Maybe a
third1 xs = case myTail xs of
  Nothing -> Nothing
  Just a  -> case myTail a of
    Nothing -> Nothing
    Just b  -> myHead b

-- the same function using the Maybe monad with the >>= operator
third2 :: [a] -> Maybe a
third2 xs =
  myTail xs >>=
    (\a -> myTail a >>=
      (\b -> myHead b))

-- the same function with a different layout
third3 :: [a] -> Maybe a
third3 xs = myTail xs >>= (\a ->
            myTail a  >>= (\b ->
            myHead b))

-- Thanks to the associativity law, we can also remove unnecessary parentheses
third4 :: [a] -> Maybe a
third4 xs = myTail xs >>= \a ->
            myTail a  >>= \b ->
            myHead b

-- finally we can use the do-notation
third5 :: [a] -> Maybe a
third5 xs = do
  a <- myTail xs
  b <- myTail a
  myHead b
