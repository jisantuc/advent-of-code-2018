module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BS
import           Data.List                        (foldl')
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Lib

main :: IO ()
main = do
  inf <- BS.readFile "/home/james/aoc/puzzles/day3.txt"
  rectangles <- return $ parseRectangles inf
  g <- return $ grid rectangles
  atLeast2 <- return $ length . filter (nContainedGT2 rectangles) $ g
  print $ "Covered by at least 2: " ++ show atLeast2
  print $ "Best rectangle: " ++ (show . (shapeId <$>) . Map.keys $ (Map.filter (\x -> x == Set.singleton False) (intersectMap rectangles)))
  where
    parseRectangles inFile =
      case (parseOnly (many' (rectParser <* endOfLine)) inFile) of
        Right (rects) -> rects
        Left _        -> []
    ulx rects = foldl' (\x y -> x `min` y) 0 $ fst .  upperLeft <$> rects
    lrx rects = foldl' (\x y -> x `max` y) 0 $ fst .  lowerRight <$> rects
    uly rects = foldl' (\x y -> x `min` y) 0 $ snd .  upperLeft <$> rects
    lry rects = foldl' (\x y -> x `max` y) 0 $ snd .  lowerRight <$> rects
    grid rects = [(x,y) | x <- [(ulx rects)..(lrx rects)], y <- [(uly rects)..(lry rects)]]
