module Parser where

-- TODO: how to check comments

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Data.Ratio

import Types

parseInt :: Parsec String () LispValue
parseInt = do
  num <- many1 digit
  return $ Ratio $ toRational $ read num

parseIntMinus :: Parsec String () LispValue
parseIntMinus = do
  minus <- char '-'
  num <- many1 digit
  return $ Ratio $ toRational $ read $ mconcat [[minus], num]

parseFloat :: Parsec String () LispValue
parseFloat = do
  numerator <- many1 digit
  dot <- char '.'
  denumerator <- many1 digit
  return $ Ratio $ toRational $ read (mconcat [numerator, [dot], denumerator])

parseFloatMinus :: Parsec String () LispValue
parseFloatMinus = do
  minus <- char '-'
  numerator <- many1 digit
  dot <- char '.'
  denumerator <- many1 digit
  return $ Ratio $ toRational $ read (mconcat [[minus], numerator, [dot], denumerator])

parseRatio :: Parsec String () LispValue
parseRatio = do
  numerator <- many1 digit
  dot <- char '/'
  denumerator <- many1 digit
  return $ Ratio (read numerator % read denumerator)

parseRatioMinus :: Parsec String () LispValue
parseRatioMinus = do
  minus <- char '-'
  numerator <- many1 digit
  dot <- char '/'
  denumerator <- many1 digit
  return $ Ratio (read (mconcat [[minus], numerator]) % read denumerator)

parseBool :: Parsec String () LispValue
parseBool = Bool <$> toBool <$> (char '#' >> oneOf "tf")
  where toBool 't' = True
        toBool 'f' = False

parseString :: Parsec String () LispValue
parseString = String <$> between (char '"') (char '"') (many (noneOf "\""))

parseKeyword :: Parsec String () LispValue
parseKeyword = Keyword <$> (char '\'' >> many1 (noneOf " ;()"))
--                           SHIT ^^ HERE

parseWord :: Parsec String () LispValue
parseWord = Word <$> (many1 $ noneOf ";()\" ")

parseAtom = choice [try parseFloat,
                    try parseFloatMinus,
                    try parseRatio,
                    try parseRatioMinus,
                    try parseInt,
                    try parseIntMinus,
                    try parseBool,
                    try parseString,
                    try parseKeyword,
                    try parseWord]

parseList :: Parsec String () LispValue
parseList = List <$>
            between (char '(') (char ')') ((try parseList <|> try parseQuoted <|> parseAtom) `sepBy` spaces)

parseQuoted :: Parsec String () LispValue
parseQuoted = Quoted <$>
              (char '\''
               >> between (char '(') (char ')') ((try parseList <|> try parseQuoted  <|> parseAtom) `sepBy` spaces))
