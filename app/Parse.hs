module Parse
  ( parseExpression
  ) where

import Text.ParserCombinators.Parsec
import Numeric
import Data.Complex
import Data.Ratio

import LispValue
import Helpers


parseExpression :: Parser LispValue
parseExpression = parseAtom
                  <|> parseString
                  -- in '()'
                  <|> try parseList
                  <|> try parseDottedList
                  -- '#' prefixed
                  <|> try parseCharacter
                  <|> try parseBool
                  <|> try parsePrefixedInteger
                  -- Numbers
                  <|> try parseComplex
                  <|> try parseFloat
                  <|> try parseRational
                  <|> try parseInteger

parseAtom :: Parser LispValue
parseAtom = do
  let symbol = oneOf "!$%&|*+-/:<=>?@^_~"
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return (LispAtom atom)

parseString :: Parser LispValue
parseString = do
  char '"'
  chars <- many (escapedChar <|> nonEscapedChar)
  char '"'
  return (LispString chars)
  where
    escapedCharacters = '"':'\\':'n':""
    nonEscapedChar = noneOf escapedCharacters
    escapedChar = do
      char '\\'
      x <- oneOf escapedCharacters
      return $ case x of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> x

parseCharacter :: Parser LispValue
parseCharacter = do
  string "#\\"
  char <- try keywordCharacter <|> justCharacter
  return (LispCharacter char)
  where
    keywordCharacter = do
      keyword <- string "space" <|> string "newline"
      return $ case keyword of
        "space" -> ' '
        "newline" -> '\n'
    justCharacter = do
      char <- anyChar
      notFollowedBy alphaNum
      return char

parseBool :: Parser LispValue
parseBool = do
  char '#'
  value <- char 't' <|> char 'f'
  return $ case value of
    't' -> LispBool True
    'f' -> LispBool False

-- Numbers

parsePrefixedInteger :: Parser LispValue
parsePrefixedInteger = try parseDecimal
                       <|> try parseHex
                       <|> try parseOct
                       <|> try parseBin

parseDecimal :: Parser LispValue
parseDecimal = do
  string "#d"
  digitChars <- many1 digit
  let result = LispInteger . read $ digitChars
  return result

parseHex :: Parser LispValue
parseHex = do
  string "#x"
  hexChars <- many1 hexDigit
  let result = LispInteger . getReadValue . readHex $ hexChars
  return result

parseOct :: Parser LispValue
parseOct = do
  string "#o"
  octChars <- many1 octDigit
  let result = LispInteger . getReadValue . readOct $ octChars
  return result

parseBin :: Parser LispValue
parseBin = do
  string "#b"
  bits <- many1 (oneOf "01")
  let result = LispInteger . bitsToInteger $ bits
  return result

parseComplex :: Parser LispValue
parseComplex = do
  realPart <- try parseFloat <|> parseInteger
  char '+'
  imaginaryPart <- try parseFloat <|> parseInteger
  char 'i'
  let result = LispComplex (getLispDouble realPart :+ getLispDouble imaginaryPart)
  return result

parseRational :: Parser LispValue
parseRational = do
  integerPart <- parseDecimal
  char '/'
  fractionalPart <- parseDecimal
  let result = LispRational (getLispInteger integerPart % getLispInteger fractionalPart)
  return result

parseFloat :: Parser LispValue
parseFloat = do
  integerPart <- many digit
  char '.'
  fractionalPart <- many1 digit
  let floatChars = integerPart ++ "." ++ fractionalPart
  let result = LispFloat . getReadValue . readFloat $ floatChars
  return result

parseInteger :: Parser LispValue
parseInteger = LispInteger . read <$> many1 digit

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseList :: Parser LispValue
parseList = do
  char '('
  values <- sepBy parseExpression spaces1
  char ')'
  return (LispList values)

parseDottedList :: Parser LispValue
parseDottedList = do
  char '('
  head <- endBy parseExpression spaces
  tail <- char '.' >> spaces >> parseExpression
  char ')'
  return (LispDottedList head tail)
