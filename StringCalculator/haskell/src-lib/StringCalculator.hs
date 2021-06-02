{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module StringCalculator (add, safeAdd, NegativeNumbersException (..)) where

import Control.Exception (throw)
import qualified Data.Text as DT
import Data.Text.Read
  ( decimal,
    signed,
  )
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Read as TR

newtype NegativeNumbersException = NNException Text
  deriving newtype (Show, Eq)

instance Exception NegativeNumbersException

newtype Delimiters = Delims [Text]
  deriving newtype (Show)

type MDelimiterParser = TM.Parsec Void Text

detectDelimiters :: Text -> Either Text (Delimiters, Text)
detectDelimiters t =
  if DT.isPrefixOf "//" t
    then case TM.runParser parseCustomDelimiters "Input" t of
      Right r -> Right r
      Left _ -> Left "Error during parsing of delimiters"
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

toInts :: Delimiters -> Text -> Either Text [Int]
toInts d = mapM toInt . split d
  where
    toInt :: Text -> Either Text Int
    toInt b =
      case signed decimal b of
        Right (i, t) ->
          if DT.null t
            then Right i
            else Left "Error during conversion to int !"
        Left e -> Left (DT.pack e)

calculate :: [Int] -> Int
calculate =
  sum . filter (<= 1000)

errorOnNegativeNumbers :: [Int] -> Either Text [Int]
errorOnNegativeNumbers i =
  if any (< 0) i
    then toError $ fromInt <$> filter (< 0) i
    else Right i
  where
    toError :: [Text] -> Either Text [Int]
    toError = Left . DT.intercalate ","

    fromInt :: Int -> Text
    fromInt = DT.pack . (show :: Int -> String)

throwOnNegativeNumbers :: [Int] -> [Int]
throwOnNegativeNumbers i =
  case errorOnNegativeNumbers i of
    Right r -> r
    Left e -> throw . NNException $ e

safeAdd :: Text -> Either Text Int
safeAdd a =
  if DT.null a
    then Right 0
    else do
      b <- detectDelimiters a
      c <- uncurry toInts b
      d <- errorOnNegativeNumbers c
      pure $ calculate d

add :: Text -> Int
add "" = 0
add a =
  let (Right unsafeResult) =
        calculate . throwOnNegativeNumbers <$> (uncurry toInts =<< detectDelimiters a)
   in unsafeResult
