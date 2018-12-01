module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import           Data.List                        (find, foldl', unfoldr)
import qualified Data.Map.Strict                  as Map

directiveParser :: Parser Int
directiveParser = signed decimal

countInList :: Eq a => a -> [a] -> Int
countInList query xs = length $ filter (\x -> x == query) xs

findFirstDouble :: [Int] -> Int
findFirstDouble [] = 0
findFirstDouble xs =
  go xs Map.empty
  where
    go [] _ = 0 -- stupid placeholder, and I know my list is infinite
    go (h:t) m =
      case Map.lookup h m of
        (Just 1) -> h
        _ -> go t (Map.insert h 1 m)

main :: IO ()
main = do
  lines <- BS.readFile "/home/james/aoc/puzzles/day1.txt"
  print $ "First double: " ++ (show $ firstDouble lines)
  where
    f (Left _)   = []
    f (Right xs) = xs
    allFreqs v = scanl (+) 0 (cycle v)
    firstDouble lines
      = findFirstDouble . allFreqs . f
      $ parseOnly (many' (directiveParser <* endOfLine)) lines
