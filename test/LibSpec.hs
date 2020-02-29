module LibSpec
  ( spec
  )
where

import           Test.Hspec
import           Lib                           as L

spec :: Spec
spec = do
  describe "concatAll" $ do
    it "sould return \"\"" $ do
      L.concatAll ["",""] `shouldBe` ""

    it "sould return \"abcdef\"" $ do
      L.concatAll ["abc","def"] `shouldBe` "abcdef"

  describe "cycleSucc" $ do
    it "should return minBound :: Int" $ do
      L.cycleSucc (maxBound :: Int) `shouldBe` (minBound :: Int)

  describe "fib" $ do
    it "should return 0" $ do
      L.fib 0 `shouldBe` 0

    it "should return 1" $ do
      L.fib 1 `shouldBe` 1

    it "should return 1" $ do
      L.fib 2 `shouldBe` 1

    it "should return 2" $ do
      L.fib 3 `shouldBe` 2

    it "should return 3" $ do
      L.fib 4 `shouldBe` 3

    it "should return 5" $ do
      L.fib 5 `shouldBe` 5

    it "should return 8" $ do
      L.fib 6 `shouldBe` 8

    it "should return 13" $ do
      L.fib 7 `shouldBe` 13

  describe "fib'" $ do
    it "should return 0" $ do
      L.fib' 0 `shouldBe` 0

    it "should return 1" $ do
      L.fib' 1 `shouldBe` 1

    it "should return 1" $ do
      L.fib' 2 `shouldBe` 1

    it "should return 2" $ do
      L.fib' 3 `shouldBe` 2

    it "should return 3" $ do
      L.fib' 4 `shouldBe` 3

    it "should return 5" $ do
      L.fib' 5 `shouldBe` 5

    it "should return 8" $ do
      L.fib' 6 `shouldBe` 8

    it "should return 13" $ do
      L.fib' 7 `shouldBe` 13

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

  describe "isPalindrome" $ do
    it "should return \"A man a plan a canal Panama\"" $ do
      L.isPalindrome "A man a plan a canal Panama" `shouldBe` True

  describe "myDrop" $ do
    it "should return \"\"" $ do
      L.myDrop 3 "" `shouldBe` ""

    it "should return [3,4,5]" $ do
      L.myDrop 2 [1 .. 5] `shouldBe` [3 .. 5]

  describe "myElem" $ do
    it "should return False" $ do
      L.myElem 'A' "" `shouldBe` False

    it "should return False" $ do
      L.myElem 'A' "BCDEF" `shouldBe` False

    it "should return True" $ do
      L.myElem 'A' "ABCDE" `shouldBe` True

  describe "myGCD" $ do
    it "should return 6" $ do
      L.myGCD 12 18 `shouldBe` 6

    it "should return 6" $ do
      L.myGCD 12 18 `shouldBe` 6

    it "should return 1" $ do
      L.myGCD 5 7 `shouldBe` 1

    it "should return 1" $ do
      L.myGCD 5 1 `shouldBe` 1

  describe "myLength" $ do
    it "should return 0" $ do
      L.myLength [] `shouldBe` 0

    it "should return 5" $ do
      L.myLength "abcde" `shouldBe` 5

  describe "myProduct" $ do
    it "should return 24" $ do
      L.myProduct [1,2,3,4] `shouldBe` 24

  describe "myReverse" $ do
    it "should return \"\"" $ do
      L.myReverse "" `shouldBe` ""

    it "should return \"edcba\"" $ do
      L.myReverse "abcde" `shouldBe` "edcba"

  describe "myReverse'" $ do
    it "should return \"\"" $ do
      L.myReverse' "" `shouldBe` ""

    it "should return \"edcba\"" $ do
      L.myReverse' "abcde" `shouldBe` "edcba"

  describe "myTali" $ do
    it "should return \"\"" $ do
      L.myTail "" `shouldBe` ""

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

  describe "printDouble" $ do
    it "should retuen \"8\"" $ do
      L.printDouble 4 `shouldBe` "8"

  describe "remove" $ do
    it "should return \"\"" $ do
      L.remove (\x -> x == 'A') "" `shouldBe` ""

    it "should return \"ABDE\"" $ do
      L.remove (\x -> x == 'C') "ABCDE" `shouldBe` "ABDE"

  describe "remove'" $ do
    it "should return \"\"" $ do
      L.remove' (\x -> x == 'A') "" `shouldBe` ""

    it "should return \"ABDE\"" $ do
      L.remove' (\x -> x == 'C') "ABCDE" `shouldBe` "ABDE"

  describe "roll" $ do
    it "should return S3" $ do
      (L.roll 2 :: FiveSideDie) `shouldBe` L.S3

    it "should return S3" $ do
      (L.roll 7 :: FiveSideDie) `shouldBe` L.S3

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
