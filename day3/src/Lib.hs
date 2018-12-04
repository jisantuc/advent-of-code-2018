module Lib
    where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set

type Point = (Int, Int)

data Rectangle = Rectangle { upperLeft  :: !Point
                           , lowerRight :: !Point
                           , shapeId    :: !Int } deriving (Eq, Show)

instance Ord Rectangle where
  (<=) r1 r2 = shapeId r1 <= shapeId r2

intersects :: Rectangle -> Rectangle -> Bool
intersects r1@(Rectangle ul1 lr1 _) r2@(Rectangle ul2 lr2 _) =
  ul1 `containedBy` r2 ||
  lr1 `containedBy` r2 ||
  ul2 `containedBy` r1 ||
  lr2 `containedBy` r1

rectParser :: Parser Rectangle
rectParser = do
  shp <- char '#' *> decimal <* string " @ "
  ulx <- decimal
  uly <- char ',' *> decimal
  _ <- string ": "
  width <- decimal
  height <- char 'x' *> decimal
  return $ Rectangle (ulx, uly) (ulx + width - 1, uly + height - 1) shp

containedBy :: Point -> Rectangle -> Bool
containedBy p r =
  -- between xes
  fst (upperLeft r) <= fst p && fst (lowerRight r) >= fst p &&
  -- and between ys
  snd (upperLeft r) <= snd p && snd (lowerRight r) >= snd p

nContainedGT2 :: [Rectangle] -> Point -> Bool
nContainedGT2 rects p = go p rects 0
  where
    go _ [] n = n >= 2
    go _ _ 2 = True
    go point (x:xs) n = if (containedBy point x) then (go point xs (n + 1)) else (go point xs n)

mkGrid :: Int -> Int -> [Point]
mkGrid x y = [(p1, p2) | p1 <- [0..x], p2 <- [0..y]]

pointsInRect :: Rectangle -> [Point]
pointsInRect (Rectangle ul lr _) = 
  [(x, y) | x <- [fst ul .. fst lr], y <- [snd ul .. snd lr]]

intersectMap :: [Rectangle] -> Map.Map Rectangle (Set.Set Bool)
intersectMap rects =
  Map.fromList $
  [(r, Set.fromList ((nContainedGT2 rects) <$> (pointsInRect r))) | r <- rects]
