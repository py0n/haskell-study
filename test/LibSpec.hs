module LibSpec
  ( spec
  )
where

import           Test.Hspec
import           Lib                           as L

spec :: Spec
spec = do
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
