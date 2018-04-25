-- Try to understand monads

ioint = do
  putStr "Enter a number: "
  numstr <- getLine
  let numnum = read numstr :: Int
  return numnum
