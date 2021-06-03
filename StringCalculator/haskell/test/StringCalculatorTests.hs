module StringCalculatorTests (stringCalculatorKata) where

import           Control.Exception (evaluate)
import qualified StringCalculator  as SC
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

tests :: Spec
tests = do
  step1
  step2
  step3
  step4
  step5
  step6
  step7
  step8
  step9

stringCalculatorKata :: IO TestTree
stringCalculatorKata =
  testGroup "String Calculator Kata" <$> testSpecs tests

step1 :: Spec
step1 = do
  describe "Step 1 - safeAdd" $ do
    it "should return 0 when given empty string" $
      SC.safeAdd "" `shouldBe` Right 0

    it "should return 1 when given string '1'" $
      SC.safeAdd "1" `shouldBe` Right 1

    it "should return 3 when given string '1,2'" $
      SC.safeAdd "1,2" `shouldBe` Right 3

  describe "Step 1 - add" $ do
    it "should return 0 when given empty string" $
      SC.add "" `shouldBe` 0

    it "should return 1 when given string '1'" $
      SC.add "1" `shouldBe` 1

    it "should return 3 when given string '1,2'" $
      SC.add "1,2" `shouldBe` 3

-- Do some property based testing
step2 :: Spec
step2 = do
  xdescribe "Step 2 - add" $ do
    it "Any length of numbers" $
      True `shouldBe` False

  xdescribe "Step 2 - safeAdd" $ do
    it "Any length of numbers" $
      True `shouldBe` False

step3 :: Spec
step3 = do
  describe "Step 3 - add" $ do
    it "should return 6 when given '1\\n2,3'" $
      SC.add "1\n2,3" `shouldBe` 6

  describe "Step 3 - safeAdd" $ do
    it "should return 6 when given '1\\n2,3'" $
      SC.safeAdd "1\n2,3" `shouldBe` Right 6

step4 :: Spec
step4 = do
  describe "Step 4 - add" $
    it "should support delimiter change and return 3 when given string '//;\\n1;2'" $
      SC.add "//;\n1;2" `shouldBe` 3

  describe "Step 4 - safeAdd" $
    it "should support delimiter change and return 3 when given string '//;\\n1;2'" $
      SC.safeAdd "//;\n1;2" `shouldBe` Right 3

step5 :: Spec
step5 = do
  describe "Step 5 - add" $
    it "should throw when given negative numbers" $
    evaluate (SC.add "-1,-2,-3") `shouldThrow` (== SC.NNException "-1,-2,-3")

  describe "Step 5 - safeAdd" $
    it "should fail when given negative numbers" $
      SC.safeAdd "-1,2,-3" `shouldBe` Left "-1,-3"

step6 :: Spec
step6 = do
  describe "Step 6 - add" $
    it "should ignore numbers greater than 1000" $
      SC.add "//;\n2;1001" `shouldBe` 2

  describe "Step 6 - safeAdd" $
    it "should ignore numbers greater than 1000" $
      SC.safeAdd "//;\n2;1001" `shouldBe` Right 2

step7 :: Spec
step7 = do
  describe "Step 7 - add" $
    it "should support delimiter of any size" $
      SC.add "//[***]\n1***2***3" `shouldBe` 6

  describe "Step 7 - safeAdd" $
    it "should support delimiter of any size" $
      SC.safeAdd "//[***]\n1***2***3" `shouldBe` Right 6

step8 :: Spec
step8 = do
  describe "Step 8 - add" $
    it "should support multiple delimiters" $
      SC.add "//[*][%]\n1*2%3" `shouldBe` 6

  describe "Step 8 - safeAdd" $
    it "should support multiple delimiters" $
    SC.safeAdd "//[*][%]\n1*2%3" `shouldBe` Right 6

step9 :: Spec
step9 = do
  describe "Step 9 - add" $
    it "should support multiple delimiters of any length" $
      SC.add "//[***][%]\n1***2%3" `shouldBe` 6

  describe "Step 9 - safeAdd" $
    it "should support multiple delimiters of any length" $
      SC.safeAdd "//[***][%]\n1***2%3" `shouldBe` Right 6
