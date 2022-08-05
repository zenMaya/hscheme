module Core where

import Control.Monad.State (liftIO)
import qualified Data.Map.Strict as Map
import Types
import qualified Data.Vector
import Prelude hiding (pred)
import Control.Monad.Trans.Except (throwE)

-- Helpers

pred :: (Scm a) => String -> ([ScmValue] -> a) -> (String, ScmValue)
pred name f = (name, ScmFunction (return . Just . scmFrom . f))

predM name f = (name, ScmFunction (return . f))

pred1 :: (Scm a) => String -> (ScmValue -> a) -> (String, ScmValue)
pred1 name f = (name, ScmFunction (\[v] -> return . Just $ scmFrom $ f v))

pred2 :: (Scm a) => String -> (ScmValue -> ScmValue -> a) -> (String, ScmValue)
pred2 name f = (name, ScmFunction (\[a, b] -> return $ Just $ scmFrom $ f a b))

pred2R :: (Scm a) => String -> (ScmValue -> ScmValue -> [ScmValue] -> a) -> (String, ScmValue)
pred2R name f = (name, ScmFunction (\(a:b:r) -> return $ Just $ scmFrom $ f a b r))

pred2All :: String -> (ScmValue -> ScmValue -> Bool) -> (String, ScmValue)
pred2All name f = (name, ScmFunction (return . Just . scmFrom . pred2AllInner f))
  where
    pred2AllInner f [_] = True
    pred2AllInner f (x:y:xs) = if f x y then pred2AllInner f (y:xs) else False

-- Base Types

isBoolean :: ScmValue -> Bool
isBoolean (ScmBoolean _) =  True
isBoolean _ =  False

isSymbol :: ScmValue -> Bool
isSymbol (ScmSymbol _) =  True
isSymbol _ =  False

isNumber :: ScmValue -> Bool
isNumber (ScmInteger _) =  True
isNumber (ScmRational _ _) =  True
isNumber (ScmReal _) =  True
isNumber (ScmComplex _ _) =  True
isNumber _ =  False

isInteger :: ScmValue -> Bool
isInteger (ScmInteger _) = True
isInteger _ = False

isChar :: ScmValue -> Bool
isChar (ScmCharacter _) =  True
isChar _ =  False

isString :: ScmValue -> Bool
isString (ScmString _) = True
isString _ = False

isList :: ScmValue -> Bool
isList (ScmList _) =  True
isList (ScmPair _ _) = False

isPair :: ScmValue -> Bool
isPair (ScmPair _ _) = True
isPair _ =  False

isNull :: ScmValue -> Bool
isNull (ScmList []) =  True
isNull _ = False

isProcedure :: ScmValue -> Bool
isProcedure (ScmFunction _) = True
isProcedure _ = False

-- Base list operations

car :: ScmValue -> ScmReturn ScmValue
car (ScmPair (car:cdr) end) = return car
car (ScmPair [] _) = return $ ScmList []
car (ScmList (car:cdr)) = return car
car s = throwE ("Value `" ++ show s ++ "` is not a list")

cdr :: ScmValue -> ScmValue
cdr (ScmPair (car:cdr) end) = ScmPair cdr end
cdr (ScmPair [] cdr) = cdr
cdr (ScmList (car:cdr)) = ScmList cdr

cons :: ScmValue -> ScmValue -> ScmValue
cons a (ScmList l) = ScmList (a:l)
cons a (ScmPair l d) = ScmPair (a:l) d
cons a b = ScmPair [a] b

length :: ScmValue -> ScmValue
length (ScmList l) = scmFrom $ Prelude.length l

append :: [ScmValue] -> ScmValue
append (l:lists) = innerAppend l lists
  where
    innerAppend (ScmList []) [b] = b
    innerAppend (ScmList a) [(ScmList b)] = ScmList (a ++ b)
    innerAppend (ScmList a) [b] = ScmPair a b
    innerAppend (ScmList a) ((ScmList b):rest) = innerAppend (ScmList (a ++ b)) rest

last :: [ScmValue] -> Maybe ScmValue
last [] = Nothing
last l = (Just . Prelude.last) l

-- Arithmetic
add :: [ScmValue] -> ScmValue
add = foldl1 innerAdd
  where
    innerAdd :: ScmValue -> ScmValue -> ScmValue
    innerAdd (ScmInteger a) (ScmInteger b) = ScmInteger $ a + b

sub :: [ScmValue] -> ScmValue
sub = foldl1 innerSub
  where
    innerSub (ScmInteger a) (ScmInteger b) = ScmInteger $ a - b

mul :: [ScmValue] -> ScmValue
mul = foldl1 innerMul
  where
    innerMul (ScmInteger a) (ScmInteger b) = ScmInteger $ a * b

div' :: [ScmValue] -> ScmValue
div' = foldl1 innerDiv
  where
    innerDiv (ScmInteger a) (ScmInteger b) = ScmInteger $ a `div` b

-- Logical

($=) :: ScmValue -> ScmValue -> Bool
($=) (ScmInteger a) (ScmInteger b) = a == b
($=) _ _ = False

-- this function currently does nothing, as Hscheme is missing
-- any kind of garbage collection, and does everything
-- in a dumb way. And haskell by itself doesn't allow to check
-- referential identity of objects
eq :: ScmValue -> ScmValue -> Bool
eq a b = a $= b


-- Same as eq, but at least works for primitives
-- TODO: other number types
eqv :: ScmValue -> ScmValue -> Bool
eqv (ScmBoolean a) (ScmBoolean b) = a == b
eqv (ScmCharacter a) (ScmCharacter b) = a == b
eqv (ScmString a) (ScmString b) = a == b
eqv a b = eq a b

equal :: ScmValue -> ScmValue -> Bool
equal (ScmList a) (ScmList b) = and $ zipWith equal a b
equal (ScmVector a) (ScmVector b) = and $ Data.Vector.zipWith equal a b
equal (ScmPair a b) (ScmPair c d) = and (zipWith equal a c) && equal b d
equal a b = eqv a b

($>) :: ScmValue -> ScmValue -> Bool
($>) (ScmInteger a) (ScmInteger b) = a > b

($<) :: ScmValue -> ScmValue -> Bool
($<) (ScmInteger a) (ScmInteger b) = a < b

($>=) :: ScmValue -> ScmValue -> Bool
($>=) a b = a $> b || a $= b

($<=) :: ScmValue -> ScmValue -> Bool
($<=) a b = a $< b || a $= b

not :: ScmValue -> Bool
not (ScmBoolean False) = True
not _ = False

-- Testing

assert :: [ScmValue] -> ScmReturn ()
assert [ScmBoolean False] = throwE "Assert failed"
assert _ = pure ()

prelude :: [(String, ScmValue)]
prelude = [
  ("car", ScmFunction (\[a] -> Just <$> car a)),
  ("cdr", ScmFunction (\[a] -> return $ Just $ cdr a)),
  (pred2 "cons" cons),
  (pred1 "length" Core.length),
  (pred "append" append),
  (predM "last" Core.last),
  (predM "begin" Core.last),
  (pred1 "boolean?" isBoolean),
  (pred1 "symbol?" isSymbol),
  (pred1 "pair?" isPair),
  (pred1 "number?" isNumber),
  (pred1 "integer?" isInteger),
  (pred1 "char?" isChar),
  (pred1 "string?" isString),
  (pred1 "list?" isList),
  (pred1 "pair?" isPair),
  (pred1 "null?" isNull),
  (pred1 "procedure?" isProcedure),
  (pred "+" add),
  (pred "-" sub),
  (pred "*" mul),
  (pred "/" div'),
  (pred2All "=" ($=)),
  (pred2All "eq?" eq),
  (pred2All "eqv?" eqv),
  (pred2All "equal?" equal),
  (pred2All ">" ($>)),
  (pred2All "<" ($<)),
  (pred2All ">=" ($>=)),
  (pred2All "<=" ($<=)),
  (pred1 "not" Core.not),
  ("assert", ScmFunction (\args -> Nothing <$ assert args))
  ]
