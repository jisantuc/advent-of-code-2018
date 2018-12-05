module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BS
import           Data.List                        (sort)
import           Lib

main :: IO ()
main = do
  inf <- BS.readFile "/home/james/aoc/puzzles/day4.txt"
  records <- return . sort . f $ parseOnly (many' (recordParser <* endOfLine)) inf
  print $ "Evenly divided records: " ++ show (mod (length records) 3 == 0)
  where
    f x =
      case x of
        Right xs -> xs
        Left _ -> []
