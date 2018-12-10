module Lib where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Owner = Char

data Point = Point !(Int, Int)
  | TaggedPoint !(Int, Int) Owner
  | OwnedPoint !(Int, Int) (Maybe Owner) deriving (Show)

instance Eq Point where
  (==) (Point p1) (Point p2) = p1 == p2
  (==) p1 p2                 = toUntaggedPoint p1 == toUntaggedPoint p2

toUntaggedPoint :: Point -> Point
toUntaggedPoint p@(Point _)            = p
toUntaggedPoint (OwnedPoint coords _)  = Point coords
toUntaggedPoint (TaggedPoint coords _) = Point coords

tag :: Point -> Grid -> Point
tag tp@(TaggedPoint _ _) _ = tp
tag op@(OwnedPoint _ _) _  = op
tag pt@(Point coords) g    = OwnedPoint coords $ owner pt g

isTagged :: Point -> Bool
isTagged (Point _) = False
isTagged _         = True

{- This ord instnace is important for some set member equality calculations
-}
instance Ord Point where
  compare p1@(Point (x1, y1)) p2@(Point (x2, y2)) =
    if (p1 == p2) then EQ
    else
      if (sumSquares1 == sumSquares2) then
        if (x1 == x2) then
          compare y1 y2
        else
          compare x1 x2
      else
        compare sumSquares1 sumSquares2
    where
      sumSquares1 = (x1 * x1 + y1 * y1)
      sumSquares2 = (x2 * x2 + y2 * y2)
  compare x y = compare (toUntaggedPoint x) (toUntaggedPoint y)

newtype Grid = Grid (Point -> Point)

type Distance = Int

buildNeighborhood :: Distance -> Point -> Set.Set Point
buildNeighborhood 0 p = Set.singleton p
buildNeighborhood n (Point (x, y)) =
  mconcat newNeighborhoods
  where
    newNeighborhoods =
      buildNeighborhood (n - 1) <$> [ Point (x - 1, y)
                                    , Point (x + 1, y)
                                    , Point (x, y - 1)
                                    , Point (x, y + 1) ]
buildNeighborhood n tagged =
  buildNeighborhood n $ toUntaggedPoint tagged


searchNearby :: Distance -> Point -> Grid -> [Point]
searchNearby n p (Grid f) =
  f <$> Set.toList (buildNeighborhood n p)

owner :: Point -> Grid -> Maybe Char
owner (TaggedPoint _ o) _ = Just o
owner point grid =
  go point grid 1
  where
    go p g n =
      case filter isTagged $ searchNearby n p g of
        []                -> go p g (n + 1)
        [TaggedPoint _ o] -> Just o
        _                 -> Nothing

gridFromPointList :: [Point] -> Grid
gridFromPointList points = Grid $
  (\(Point coords) ->
     case Map.lookup coords pointMap of
       Nothing -> Point coords
       Just p  -> p)
  where
    pointMap = Map.fromList (zip (pointKey <$> points) points)
    pointKey (TaggedPoint coords _) = coords
    pointKey (OwnedPoint coords _) = coords
    pointKey (Point coords)         = coords
