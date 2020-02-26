module LibSpec
  ( spec
  )
where

import           Test.Hspec
import           Lib                           as L

spec :: Spec
spec = do
  describe "inFirstHalf" $ do
    it "should return True" $ do
      L.inFirstHalf 'a' "abcde" `shouldBe` True

    it "should return True" $ do
      L.inFirstHalf 'b' "abcde" `shouldBe` True

    it "should return True" $ do
      L.inFirstHalf 'c' "abcde" `shouldBe` False

    it "should return True" $ do
      L.inFirstHalf 'd' "abcde" `shouldBe` False

    it "should return True" $ do
      L.inFirstHalf 'e' "abcde" `shouldBe` False

    it "should return True" $ do
      L.inFirstHalf 'f' "abcde" `shouldBe` False

  describe "myTali" $ do
    it "should return []" $ do
      L.myTail [1] `shouldBe` []

    it "should return \"ello\"" $ do
      L.myTail "Hello" `shouldBe` "ello"

  describe "myTake" $ do
    it "should return \"\"" $ do
      L.myTake 2 "" `shouldBe` ""

    it "should return \"\"" $ do
      L.myTake 0 "abcde" `shouldBe` ""

    it "should return \"ab\"" $ do
      L.myTake 2 "abcde" `shouldBe` "ab"

    it "should return \"abcde\"" $ do
      L.myTake 5 "abcde" `shouldBe` "abcde"

    it "should return \"abcde\"" $ do
      L.myTake 6 "abcde" `shouldBe` "abcde"

  describe "subseq" $ do
    it "should return [3,4,5]" $ do
      L.subseq 2 5 [1 .. 10] `shouldBe` [3, 4, 5]

    it "should return []" $ do
      L.subseq 0 0 [1 .. 10] `shouldBe` []

    it "should return []" $ do
      L.subseq 0 9 [1 .. 10] `shouldBe` [1 .. 9]

    it "should return []" $ do
      L.subseq 0 10 [1 .. 10] `shouldBe` [1 .. 10]

    it "should return \"puppy\"" $ do
      L.subseq 2 7 "a puppy" `shouldBe` "puppy"
