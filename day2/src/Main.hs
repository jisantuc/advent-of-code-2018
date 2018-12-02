module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict as Map
import           Data.Word       (Word8)

charCounts :: [Char] -> Map.Map Char Int
charCounts bs = go bs Map.empty
  where
    go [] m = m
    go (h:t) m = go t (Map.insertWith (+) h 1 m)

{- Check whether a bytestring has any characters appearing exactly n times -}
checkN :: Int -> [Char] -> Bool
checkN n bs = Map.foldl (\bool x -> bool || x == n) False (charCounts bs)

checkSum :: [[Char]] -> Int
checkSum [] = 0
checkSum xs = go xs 0 0
  where
    go [] twos threes = twos * threes
    go (x:xs) twos threes =
      case (checkN 2 x, checkN 3 x) of
        (True, True)  -> go xs (twos + 1) (threes + 1)
        (True, False) -> go xs (twos + 1) threes
        (False, True) -> go xs twos (threes + 1)
        _             -> go xs twos threes

nDiffLetters :: [Char] -> [Char] -> Int
nDiffLetters xs ys = length . filter (== True) $ zipWith (/=) xs ys

{- Two strings are buddies if they differ by exactly one letter-}
findBuddy :: [Char] -> [[Char]] -> Maybe ([Char], [Char])
findBuddy x [] = Nothing
findBuddy x xs =
  case (filter (\y -> nDiffLetters x y == 1) xs) of
    [] -> Nothing
    ys | length ys == 1 -> Just (x, head ys)
    ys -> error ("Something went wrong -- " ++ show (length ys))

buddySearch :: [[Char]] -> Maybe ([Char], [Char])
buddySearch [] = Nothing
buddySearch (x:xs) =
  case (findBuddy x xs) of
    result@(Just _) -> result
    _ -> buddySearch xs

dumbTest1 :: Bool
dumbTest1 =
  checkSum ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"] == 12

dumbTest2 :: Bool
dumbTest2 =
  findBuddy "fghij" ["abcde", "klmno", "pqrst", "fguij", "axcye", "wvxyz"] == Just ("fghij", "fguij")

dumbTest3 :: Bool
dumbTest3 =
  buddySearch ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"] == Just ("fghij", "fguij")

main :: IO ()
main = do
  inf <- readFile "/home/james/aoc/puzzles/day2.txt"
  putStrLn $ "Checksum is: " ++ show (checkSum $ lines inf)
  putStrLn $ "Same letters in buddy strings are: " ++ sameLetters inf
  where
    sameLetters inFile =
      case buddySearch (lines inFile) of
        Just (s1, s2) ->
          [fst t | t <- s1 `zip` s2, fst t == snd t]
        Nothing ->
          "something has gone horribly wrong!"

