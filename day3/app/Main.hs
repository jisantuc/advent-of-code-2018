module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BS
import           Data.List                        (foldl')
import           Lib

main :: IO ()
main = do
  inf <- BS.readFile "/home/james/aoc/puzzles/day3.txt"
  rectangles <- return $ parseRectangles inf
  print $ "Number of rectangles: " ++ show (length rectangles)
  where
    parseRectangles inFile =
      case (parseOnly (many' (rectParser <* endOfLine)) inFile) of
        Right (rects) -> rects
        Left _        -> []
    minULx rects = foldl' (\x y -> x `min` y) 0 $ fst .  upperLeft <$> rects
    maxLRx rects = foldl' (\x y -> x `min` y) 0 $ fst .  lowerRight <$> rects
    minULy rects = foldl' (\x y -> x `min` y) 0 $ snd .  upperLeft <$> rects
    maxURy rects = foldl' (\x y -> x `min` y) 0 $ snd .  lowerRight <$> rects
