import           Lib
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser test" $ do
    it "should parse sleep records" $ do
      parseOnly recordParser "[1518-11-01 00:05] falls asleep" `shouldBe`
        Right (Record (Time 1518 11 1 0 5) Sleep)

