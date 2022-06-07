{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Control.Monad (liftM)
import Control.Monad.State (runStateT, MonadState (put))
import Control.Monad.State (mapState)
import Control.Monad.State (mapStateT)
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Trans.Except
import Control.Monad.State (get)
import Control.Monad.State (withStateT)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Text.Parsec (Stream (uncons))
import Data.List (intersperse)

type Env = [IORef (Map.Map String ScmValue)]

emptyEnv :: IO Env
emptyEnv = do
  e <- newIORef Map.empty
  return [e]

newEnv :: ScmReturn ()
newEnv = do
  es <- get
  e <- liftIO $ newIORef Map.empty
  put (e:es)

leaveEnv :: ScmReturn ()
leaveEnv = do
  (e:es) <- get
  put es

insertEnv :: String -> ScmValue -> ScmReturn ()
insertEnv k v = do
  (e:es) <- get
  liftIO $ modifyIORef e $ Map.insert k v

getEnv :: String -> ScmReturn (Maybe ScmValue)
getEnv k = do
  ee <- get
  case ee of
    [] -> return Nothing
    (e:es) -> do
      m <- liftIO $ readIORef e
      case Map.lookup k m of
        Nothing -> do
          put es
          v <- getEnv k
          put (e:es)
          return v
        v -> return v

data ScmExtension
  = ScmPatternVar ScmValue
  | ScmLiteral String
  | ScmEllipsis [ScmValue]

data ScmValue
  = ScmBoolean Bool
  | ScmInteger Integer
  | ScmRational Integer Integer
  | ScmReal Float
  | ScmComplex Float Float
  | ScmSymbol String
  | ScmCharacter Char
  | ScmString String
  | ScmPair [ScmValue] ScmValue
  | ScmList [ScmValue]
  | ScmVector (V.Vector ScmValue)
  | ScmFunction ([ScmValue] -> ScmReturn (Maybe ScmValue))

instance Show ScmValue where
  show (ScmBoolean True) = "#t"
  show (ScmBoolean False) = "#f"
  show (ScmSymbol x) = x
  show (ScmCharacter x) = "'" ++ show x ++ "'"
  show (ScmString x) = "\"" ++ x ++ "\""
  show (ScmPair a b) = "(" ++ concat (intersperse " " (map show a)) ++ " . " ++ show b ++ ")"
  show (ScmList l) = "(" ++ concat (intersperse " " (map show l)) ++ ")"
  show (ScmVector x) = "#(" ++ concat (intersperse " " (map show (V.toList x))) ++ ")"
  show (ScmFunction x) = "<function>"
  show (ScmInteger i) = show i

instance {-# OVERLAPS #-} Show (Maybe ScmValue) where
  show (Just v) = show v
  show Nothing = ""

class Scm a where
  scmFrom :: a -> ScmValue
  scmTo :: ScmValue -> a
  
fromScmInteger (ScmInteger a) = a

fromScmRational (ScmRational a b) = (a, b)

fromScmReal (ScmReal a) = a

type ScmReturn a = ExceptT String (StateT Env IO) a
--type ScmEturn a = ExceptT String (StateT Env IO) a
      
--catchE m h = (lift $ Control.Monad.Trans.Except.catchE) m h

instance (Monad m) => Stream  ScmValue m ScmValue where
  uncons (ScmList (x:xs)) = return $ Just $ (x, ScmList xs)
  uncons (ScmList []) = return Nothing
  uncons _ = return Nothing

data ScmConvertible = forall s. Scm s => ScmC s

instance Scm ScmConvertible where
  scmFrom = scmFrom
  scmTo = scmTo

instance Scm ScmValue where
  scmFrom = id
  scmTo = id

instance Scm Bool where
  scmFrom = ScmBoolean
  scmTo (ScmBoolean b) = b

instance Scm Int where
  scmFrom = ScmInteger . toInteger
  scmTo (ScmInteger i) = fromIntegral i

instance Scm Integer where
  scmFrom = ScmInteger
  scmTo (ScmInteger i) = i

instance Scm (Integer, Integer) where
  scmFrom (a, b) = ScmRational a b
  scmTo (ScmRational a b) = (a, b)

instance Scm [Char] where
  scmFrom = ScmString
  scmTo (ScmString s) = s

instance Scm [ScmConvertible] where
  scmFrom = ScmList . map scmFrom
  scmTo (ScmList l) = map scmTo l

instance (Scm a, Scm b) => Scm (a, b) where
  scmFrom (a, b) = ScmPair [(scmFrom a)] (scmFrom b)
  scmTo (ScmPair [a] b) = (scmTo a, scmTo b)

instance (Scm a) => Scm ([ScmConvertible], a) where
   scmFrom (a, b) = ScmPair (map scmFrom a) (scmFrom b)
   scmTo (ScmPair a b) = (map scmTo a, scmTo b)

instance (Scm a) => Scm [a] where
  scmFrom l = ScmList (map scmFrom l)
  scmTo (ScmList l) = map scmTo l
