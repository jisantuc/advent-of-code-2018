import           Data.Attoparsec.ByteString.Char8
import qualified Data.Map                         as Map
import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Rectangles test" $ do
    it "should parse rectangles correctly" $ do
      parseOnly rectParser "#123 @ 3,2: 5x4" `shouldBe` Right (Rectangle (3, 2) (7, 5) 123)

    it "should say points inside rectangles are contained" $ do
      containedBy (3, 3) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` True

    it "should say points outside rectangles are not contained" $ do
      containedBy (3, 3) (Rectangle (-1, -1) (0, 0) 1234) `shouldBe` False
      containedBy (3, 6) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` False
      containedBy (6, 3) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` False

    it "should say points on edges of rectangles are contained" $ do
      containedBy (0, 0) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` True
      containedBy (5, 5) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` True
      containedBy (5, 0) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` True
      containedBy (0, 5) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` True

    it "should say that rectangles intersect themselves" $ do
      (Rectangle (0, 0) (5, 5) 1234) `intersects` (Rectangle (0, 0) (5, 5) 1235) `shouldBe` True

    it "should count correctly if a point is covered by more than two rectangles" $ do
      nContainedGT2 (3, 3) [ Rectangle (0, 0) (5, 5) 1234
                           , Rectangle (0, 0) (5, 5) 1234 ] `shouldBe` True
      length (filter (\x -> nContainedGT2 x rects) (mkGrid 8 8)) `shouldBe` 4
    it "shouldn't lose keys when constructing an intersect map" $ do
      Map.size (intersectMap . Prelude.take 1000 $ rects) `shouldBe` 3

    where
      rects = [ Rectangle (1, 3) (4, 6) 1234
              , Rectangle (3, 1) (6, 4) 1234
              , Rectangle (5, 5) (6, 6) 1234
              ]
