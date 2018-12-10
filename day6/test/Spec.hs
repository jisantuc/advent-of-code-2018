import qualified Data.Set as Set
import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "Neighborhood builder" $ do
    it "should build neighborhoods of the right points" $ do
      buildNeighborhood 1 (Point (0, 0)) `Set.difference`
        neighborhood1Origin `shouldBe`
        Set.empty
    it "should build up neighborhoods inductively" $ do
      buildNeighborhood 2 (Point (0, 0)) `Set.difference`
        neighborhood2Origin `shouldBe`
        Set.empty
      buildNeighborhood 3 (Point (0, 0)) `Set.difference`
        neighborhood3Origin `shouldBe`
        Set.empty
    it "shouldn't drop points" $ do
      -- size of neighborhood given by 1 + 4 * n
      (Set.size $ buildNeighborhood 25 (Point (0, 0))) `shouldBe`
        (1 + 25 * 4)
  describe "Owner search" $ do
    it "should refuse to break ties" $ do
      owner (Point (0, 0)) testGrid `shouldBe` Nothing
    it "should find the owner from either side" $ do
      owner (Point (5, 0)) testGrid `shouldBe` Just 'a'
      owner (Point (0, 5)) testGrid `shouldBe` Just 'b'
    it "should work on a grid constructed from a list of points" $ do
      owner (Point (6, 0)) constructedGrid `shouldBe` Just 'a'
      owner (Point (3, -1)) constructedGrid `shouldBe` Nothing
      owner (Point (2, -3)) constructedGrid `shouldBe` Just 'b'
      owner (Point (2, 0)) constructedGrid `shouldBe` Nothing
  where
    neighborhood1Origin =
      Set.fromList [Point (1, 0), Point (-1, 0), Point (0, 1), Point (0, -1)]
    neighborhood2Origin =
        mconcat [ buildNeighborhood 1 (Point (0, 0))
                , buildNeighborhood 1 (Point (1, 0))
                , buildNeighborhood 1 (Point (-1, 0))
                , buildNeighborhood 1 (Point (0, 1))
                , buildNeighborhood 1 (Point (0, -1)) ]
    neighborhood3Origin =
      mconcat $ buildNeighborhood 1 <$> Set.toList neighborhood2Origin
    testGrid = Grid dummyGrid
    dummyGrid (Point (1, 0)) = TaggedPoint (1, 0) 'a'
    dummyGrid (Point (0, 1)) = TaggedPoint (0, 1) 'b'
    dummyGrid p = p
    constructedGrid =
      gridFromPointList [ TaggedPoint (4, 0) 'a'
                        , TaggedPoint (2, -2) 'b'
                        , TaggedPoint (2, 2) 'c' ]
