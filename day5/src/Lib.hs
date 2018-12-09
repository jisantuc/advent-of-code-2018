module Lib
    ( polymerReduce
    , polymerPass
    ) where
import           Data.Char (isLower, isUpper, toLower, toUpper)

polymerPass :: Char -> String -> String
polymerPass _ [] = []
polymerPass c (x:[]) = if (x /= c && x /= toUpper c) then [ x ] else []
polymerPass c (x1:x2:xs) =
  if (x1 /= c && x1 /= toUpper c) then
    if ((isUpper x1 && toLower x1 == x2) || (isLower x1 && toUpper x1 == x2)) then
      polymerPass c xs
    else
      (x1:polymerPass c (x2:xs))
  else
    (polymerPass c (x2:xs))

polymerReduce :: Char -> String -> String
polymerReduce c s =
  if (firstPass /= s) then
    polymerReduce c firstPass
  else
    s
  where
    firstPass = polymerPass c s
