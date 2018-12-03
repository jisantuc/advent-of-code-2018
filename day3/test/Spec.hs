import           Data.Attoparsec.ByteString.Char8
import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser test" $ do
    it "should parse rectangles correctly" $ do
      parseOnly rectParser "#123 @ 3,2: 5x4" `shouldBe` Right (Rectangle (3, 2) (8, 6) 123)

    it "should say points inside rectangles are contained" $ do
      containedBy (3, 3) (Rectangle (0, 0) (5, 5) 1234) `shouldBe` True

    it "should say points outside rectangles are not contained" $ do
      containedBy (3, 3) (Rectangle (-1, -1) (0, 0) 1234) `shouldBe` False
