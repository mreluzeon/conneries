module Parser where

-- TODO: how to check comments

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Data.Ratio

import Types

parseInt :: Parsec String () LispValue
parseInt = Number <$> read <$> (many1 digit)

parseFloat :: Parsec String () LispValue
parseFloat = do
  numerator <- many1 digit
  dot <- char '.'
  denumerator <- many1 digit
  return $ Number $ read (mconcat [numerator, [dot], denumerator])

parseRatio :: Parsec String () LispValue
parseRatio = do
  numerator <- many1 digit
  dot <- char '/'
  denumerator <- many1 digit
  return $ Ratio (read numerator % read denumerator)

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
                    try parseRatio,
                    try parseInt,
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
