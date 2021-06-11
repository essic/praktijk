{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module StringCalculator (add, safeAdd, NegativeNumbersException (..)) where

import           Control.Exception       (throw)
import qualified Data.Text               as DT
import           Data.Text.Read          (decimal, signed)
import qualified Text.Megaparsec         as TM
import qualified Text.Megaparsec.Char    as TMC
import qualified Text.Read               as TR
import           Data.Either.Combinators (mapLeft)

newtype NegativeNumbersException = NNException Text
  deriving newtype (Show, Eq)

instance Exception NegativeNumbersException

newtype Delimiters = Delims [Text]
  deriving newtype (Show)

type MDelimiterParser = TM.Parsec Void Text

type Error = Text

detectDelimiters :: Text -> Either Error (Delimiters, Text)
detectDelimiters t =
  if DT.isPrefixOf "//" t
    then
      mapLeft
          (const "Error during parsing of delimiters")
          (TM.runParser  parseCustomDelimiters "Input" t)
    else Right (defaultDelimiters, t)
  where
    parseCustomDelimiters :: MDelimiterParser (Delimiters, Text)
    parseCustomDelimiters = do
      _ <- TMC.string "//"
      c <- TM.try (TM.some parseAnyLengthDelimiter <|> TM.some parseSingleCharDelimiter)
      _ <- TMC.string "\n"
      r <- TM.many TMC.printChar
      pure (Delims c, DT.pack r)

    parseSingleCharDelimiter :: MDelimiterParser Text
    parseSingleCharDelimiter = do
      c <- (TM.satisfy isNotForbiddenCustomDelimiters :: MDelimiterParser Char)
      pure . DT.singleton $ c

    parseAnyLengthDelimiter :: MDelimiterParser Text
    parseAnyLengthDelimiter = do
      _ <- TMC.string "["
      c <- TM.many (TM.satisfy isNotForbiddenCustomDelimiters :: MDelimiterParser Char)
      _ <- TMC.string "]"
      pure $ DT.pack c

    isNotForbiddenCustomDelimiters :: Char -> Bool
    isNotForbiddenCustomDelimiters = (`notElem` ['[', '\n', ']'])

    defaultDelimiters :: Delimiters
    defaultDelimiters =
      Delims $ DT.singleton <$> ['\n', ',']

-- A simpler but unsafe cast...
toInt' :: Text -> Int
toInt' = TR.read . toString

split :: Delimiters -> Text -> [Text]
split d t =
  doSplit d [t]
  where
    doSplit :: Delimiters -> [Text] -> [Text]
    doSplit (Delims []) t' = t'
    doSplit (Delims (x : xs)) t' =
      doSplit (Delims xs) (concat $ DT.splitOn x <$> t')

toInts :: Delimiters -> Text -> Either Error [Int]
toInts d = mapM toInt . split d
  where
    toInt :: Text -> Either Error Int
    toInt b =
      case signed decimal b of
        Right (i, t) ->
          if DT.null t
            then Right i
            else Left "Error during conversion to int !"
        Left e -> Left (DT.pack e)


selectPositiveNumbers :: [Int] -> Either Error [Int]
selectPositiveNumbers i =
  if any (< 0) i
    then toError $ fromInt <$> filter (< 0) i
    else Right i
  where
    toError :: [Text] -> Either Error [Int]
    toError = Left . DT.intercalate ","

    fromInt :: Int -> Text
    fromInt = DT.pack . (show :: Int -> String)

throwOnNegativeNumbers :: [Int] -> Identity [Int]
throwOnNegativeNumbers i =
  case selectPositiveNumbers i of
    Right r -> Identity r
    Left e  -> throw . NNException $ e

calculateSum :: Functor f => ([Int] -> f [Int]) -> [Int] -> f Int
calculateSum g i = sum . filter (<= 1000) <$> g i

-- calculateSum' :: Functor f => ([Int] -> f [Int]) -> [Int] -> f Int
-- calculateSum' g = fmap (sum . filter (<= 1000)) <$> g

safeAdd :: Text -> Either Error Int
safeAdd a =
  if DT.null a
    then Right 0
    else do
      b <- detectDelimiters a
      c <- uncurry toInts b
      doSum c
  where
    doSum = calculateSum selectPositiveNumbers

add :: Text -> Int
add "" = 0
add a =
  let doSum = calculateSum throwOnNegativeNumbers
      (Right unsafeResult) =
        doSum <$> (uncurry toInts =<< detectDelimiters a)
   in runIdentity unsafeResult
