import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "Polymer reducer" $ do
    it "should reduce the test string correctly" $ do
      polymerReduce '|' "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"

    it "should reduce strings completely" $ do
      (polymerPass '|' . polymerReduce '|') "dabAcCaCBAcCcaDA" `shouldBe` polymerReduce '|' "dabAcCaCBAcCcaDA"

    it "should not reduce an irreducible string" $ do
      polymerReduce '|' allLowerAs `shouldBe` allLowerAs
      polymerReduce '|' allUpperAs `shouldBe` allUpperAs
      polymerReduce '|' mixedLetters `shouldBe` mixedLetters

    it "should reduce some specific combinations correctly" $ do
      polymerReduce '|' "aA" `shouldBe` ""
      polymerReduce '|' "Aa" `shouldBe` ""
      polymerReduce '|' "abBA" `shouldBe` ""
      polymerReduce '|' "abAB" `shouldBe` "abAB"
      polymerReduce '|' "aabAAB" `shouldBe` "aabAAB"
    where
      allLowerAs = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      allUpperAs = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      mixedLetters = "aklsdjowijkasljiwqopeureJLDSjOFJOLJKJSfDF"


