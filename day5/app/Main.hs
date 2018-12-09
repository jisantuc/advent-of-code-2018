module Main where

import Data.List (sortBy)
import Lib

main :: IO ()
main = do
  inf <- readFile "/home/james/aoc/puzzles/day5.txt"
  reduced <- return $ polymerReduce '|' inf
  print $ "Reduced length, no special removal: " ++ (show (length reduced))
  print $ "Shortest chain after removal of worst reactor: " ++ (show . take 2 $ allReductions inf)
  where
    allReductions inFile =
      sortBy (\x y -> snd x `compare` snd y) $
      (\x -> (x, length $ polymerReduce x inFile)) <$> ['a'..'z']
