import StringCalculatorTests (stringCalculatorKata)
import Test.Tasty

main :: IO ()
main = do
  s <- stringCalculatorKata
  defaultMain $ testGroup "All Katas" [s]
