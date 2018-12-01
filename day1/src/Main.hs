module Main where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import Data.List (foldl')

directiveParser :: Parser Int
directiveParser = signed decimal

main :: IO ()
main = do
  lines <- BS.readFile "/home/james/aoc/puzzles/day1.txt"
  vals <- return . f $ parseOnly (many' (directiveParser <* endOfLine)) lines
  print $ "Sum is " ++ show (foldl' (+) 0 vals)
  where
    f (Left _) = []
    f (Right xs) = xs
