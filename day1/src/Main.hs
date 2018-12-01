module Main where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import           Data.List                        (find, foldl', unfoldr)

directiveParser :: Parser Int
directiveParser = signed decimal

countInList :: Eq a => a -> [a] -> Int
countInList query xs = length $ filter (\x -> x == query) xs

subsetUntilDouble :: [Int] -> Int
subsetUntilDouble [] = 0
subsetUntilDouble xs =
  go xs []
  where
    go [] _ = 0 -- stupid placeholder, and I know my list is infinite
    go (h:t) accumed
      | elem h accumed = h
      | otherwise =
        go t (h : accumed)

main :: IO ()
main = do
  lines <- BS.readFile "/home/james/aoc/puzzles/day1.txt"
  print $ "First double: " ++ (show $ firstDouble lines)
  where
    f (Left _)   = []
    f (Right xs) = xs
    allFreqs v = scanl (+) 0 (cycle v)
    firstDouble lines
      = subsetUntilDouble . allFreqs . f
      $ parseOnly (many' (directiveParser <* endOfLine)) lines
