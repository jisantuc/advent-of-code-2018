module Lib
    where

import           Data.Attoparsec.ByteString.Char8

type Point = (Int, Int)

data Rectangle = Rectangle { upperLeft :: Point
                           , lowerRight :: Point
                           , shapeId :: Int } deriving (Eq, Show)

rectParser :: Parser Rectangle
rectParser = do
  shp <- char '#' *> decimal <* string " @ "
  ulx <- decimal
  uly <- char ',' *> decimal
  _ <- string ": "
  width <- decimal
  height <- char 'x' *> decimal
  return $ Rectangle (ulx, uly) (ulx + width, uly + height) shp

containedBy :: Point -> Rectangle -> Bool
containedBy p r =
  -- between xes
  fst (upperLeft r) <= fst p && fst (lowerRight r) >= fst p &&
  -- and between ys
  snd (upperLeft r) <= snd p && snd (lowerRight r) >= snd p
