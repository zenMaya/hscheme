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
  <|> unquote_splicing
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

unquote_splicing :: Parser ScmValue
unquote_splicing = do
  string ",@"
  d <- datum
  return $ ScmList [ScmSymbol "unquote-splicing", d]

pair = do
  car <- many (datum <* spaces1)
  char '.'
  spaces1
  cdr <- datum
  return $ ScmPair car cdr

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
  -- do
  -- (base, _) <- prefix
  -- choice
  --   [ complex base,
  --     ScmReal <$> real base,
  --     rational base,
  --     ScmInteger <$> Reader.integer base
  --   ]

-- complex :: Int -> Parser ScmValue
-- complex i = do
--   re <- real i
--   spaces
--   im <- imaginary i
--   return $ ScmComplex re im

-- imaginary :: Int -> Parser Float
-- imaginary i = option 1.0 (real i) <* spaces <* char 'i'

-- real :: Int -> Parser Float
-- real i = do
--   s <- sign
--   (n, d) <- (real_ <|> dec <|> nat)
--   return 
--   where
--     real_ i = do
--       nat <- uinteger i
--       char '.'
--       dec <- uinteger i
--       return (nat, dec)
--     dec i = (0, char '.' *> uinteger i)
--     nat i = (uinteger i <* char '.', 0)

-- rational i = do
--   n <- Reader.integer i
--   char '/'
--   d <- uinteger i
--   return $ ScmRational n d

-- integer :: Int -> Parser Integer
-- integer i = do
--   s <- optionMaybe sign
--   u <- uinteger i
--   return $ case s of
--     (Just '-') -> (-u)
--     _ -> u

-- uinteger :: Int -> Parser Integer
-- uinteger i = do
--   n <- many1 $ digit' i
--   return (readInBase i n)

-- type Prefix = (Int, Exactness)

-- prefix :: Parser Prefix
-- prefix = do pre <|> post
--   where
--     pre = do
--       r <- radix
--       e <- exactness
--       return (r, e)
--     post = do
--       e <- exactness
--       r <- radix
--       return (r, e)

-- type Exponent = Integer

-- suffix :: Parser Exponent
-- suffix = exponentMarker *> Reader.integer 10

-- exponentMarker = oneOf "eEsSfFdDlL"

-- mantissaWidth = optionMaybe $ many1 $ digit' 10

-- sign = char '+' <|> char '-'

-- data Exactness = Inexact | Exact

-- exactness =
--   choice
--     [ Exact <$ (string "#e" <|> string "#E"),
--       Inexact <$ (string "#i" <|> string "#I")
--     ]

-- radix :: Parser Int
-- radix =
--   choice
--     [ 2 <$ (string "#b" <|> string "#B"),
--       8 <$ (string "#o" <|> string "#O"),
--       16 <$ (string "#x" <|> string "#X"),
--       10 <$ (string "#d" <|> string "#D" <|> string "")
--     ]

-- digit' :: Int -> Parser Char
-- digit' 2 = oneOf "01"
-- digit' 8 = octDigit
-- digit' 10 = digit
-- digit' 16 = hexDigit

space' :: Parser ()
space' =  void ( space <|> (between (try $ string "#|") (string "|#") anyChar) <|> (between (char ';') endOfLine anyChar))

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
