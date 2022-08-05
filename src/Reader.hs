module Reader (readStr, readMany) where

import Control.Monad.State (liftIO)
import Data.Char (chr, digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)
import Types
import Text.Parsec.Token
import Text.Parsec.Char (endOfLine)
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Trans.Except (throwE)

readStr :: String -> ScmReturn ScmValue
readStr str = case parse datum "Scheme" str of
  Left err -> throwE $ show err
  Right val -> return val

readMany :: String -> ScmReturn [ScmValue]
readMany str = case parse (sepEndBy datum (skipMany newline <|> spaces)) "Scheme" str of
  Left err -> throwE $ show err
  Right vals -> return vals

datum :: Parser ScmValue
datum =
  ScmBoolean <$> try (char '#' *> (True <$ char 't' <|> False <$ char 'f'))
  <|> ScmString <$> (char '"' *> many stringElement <* char '"')
  <|> char '(' *> spaces *> (try pair <|> list) <* char ')'
  <|> string "[" *> spaces *> vector <* spaces <* char ']'
  <|> try syntax
  <|> quote
  <|> quasiquote
  <|> try unquote
  <|> unquoteSplicing
  <|> peculiar
  <|> sym
  <|> number

syntax :: Parser ScmValue
syntax = do
  d <- string "#'" *> datum
  return $ ScmList [ScmSymbol "syntax", d]

stringElement :: Parser Char
stringElement = (char '\\' *>
  choice
    [ '\a' <$ char 'a',
      '\b' <$ char 'b',
      '\t' <$ char 't',
      '\n' <$ char 'n',
      '\v' <$ char 'v',
      '\f' <$ char 'f',
      '\r' <$ char 'r',
      '"' <$ char '"',
      '\\' <$ char '\\']) <|> noneOf "\"\\"

quote :: Parser ScmValue
quote = do
  char '\''
  d <- datum
  return $ ScmList [ScmSymbol "quote", d]

quasiquote :: Parser ScmValue
quasiquote = do
  char '`'
  d <- datum
  return $ ScmList [ScmSymbol "quasiquote", d]

unquote :: Parser ScmValue
unquote = do
  char ','
  d <- datum
  return $ ScmList [ScmSymbol "unquote", d]

unquoteSplicing :: Parser ScmValue
unquoteSplicing = do
  string ",@"
  d <- datum
  return $ ScmList [ScmSymbol "unquote-splicing", d]

pair = do
  car <- many (datum <* spaces1)
  char '.'
  spaces1
  ScmPair car <$> datum

list :: Parser ScmValue
list = ScmList <$> sepBy datum spaces1

vector =
  ScmVector . V.fromList <$> sepBy datum spaces1

peculiar = ScmSymbol <$> try (string "+" <|> string "-" <|> string "..." <|> arrow)
  where
    arrow :: Parser String
    arrow = do
      a <- string "->"
      s <- many subsequent
      return (a ++ s)

sym :: Parser ScmValue
sym = do
  ini <- initial
  sub <- many subsequent
  return $ ScmSymbol (ini:sub)

subsequent = initial <|> alphaNum <|> specialSubsequent
initial = letter <|> specialInitial
specialInitial = oneOf "!$%&*/:<=>?^_~"
specialSubsequent = oneOf "+-.@"

--TODO: add other number types
number :: Parser ScmValue
number = do
  d <- many1 digit
  return $ ScmInteger (read d)

space' :: Parser ()
space' =  void ( space <|> between (try $ string "#|") (string "|#") anyChar <|> between (char ';') endOfLine anyChar)

spaces1 :: Parser ()
spaces1 = skipMany1 space'

spaces :: Parser ()
spaces = skipMany space'

-- Haskell Specific
readInBase :: Int -> [Char] -> Integer
readInBase b n = readInBase_ b n 0
  where
    readInBase_ :: Int -> [Char] -> Integer -> Integer
    readInBase_ b [] acc = acc
    readInBase_ b (x : xs) acc = readInBase_ b xs $ acc * toInteger b + toInteger (digitToInt x)

readFloat :: Integer -> Integer -> Integer -> Integer -> Float
readFloat s d r e = read $ show (d * e * toInteger s) ++ "." ++ show r
