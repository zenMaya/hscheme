module Repl (scmPrint, scmDisplay, scmRead, eval, re) where

import Control.Monad.State (execStateT, get, liftIO, liftM, modify, withStateT)
import Reader
import Types
import Control.Monad
import Data.Maybe (isJust, catMaybes)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans (lift)
import Control.Monad.Except (mapExceptT)
import Control.Monad.Trans.State (put)
import qualified Data.Map.Strict as Map
import Data.List
import Data.IORef (readIORef)
import GHC.Stack (errorWithStackTrace)

quasiquote :: ScmValue -> ScmReturn [ScmValue]
quasiquote (ScmList []) = return []
quasiquote (ScmList ((ScmList ((ScmSymbol "unquote"):v:_)):rest)) = do
  rest <- quasiquote (ScmList rest)
  v <- eval v
  case v of
    Just v -> return (v:rest)
    _ -> return rest
quasiquote (ScmList ((ScmList ((ScmSymbol "unquote-splicing"):v:_)):rest)) = do
  rest <- quasiquote (ScmList rest)
  Just (ScmList v) <- eval v
  return (v ++ rest)
quasiquote (ScmList (s@(ScmSymbol _):cdr)) = do
  let car = ScmList [ScmSymbol "quote", s]
  cdr <- quasiquote (ScmList cdr)
  return (car:cdr)
quasiquote (ScmList (car:cdr)) = do
  cdr <- quasiquote (ScmList cdr)
  return (car:cdr)

scmIf :: [ScmValue] -> ScmReturn (Maybe ScmValue)
scmIf (cond:t:f:[]) = do
  cond <- eval cond
  case cond of
    Just (ScmBoolean True) -> eval t
    Just (ScmBoolean False) -> eval f
    Just v -> throwE $ "Condition must evaluate to a boolean value, it was `" ++ show v ++ "`"
    Nothing -> throwE "Condition must evaluate to a boolean value!"
scmIf (cond:t:[]) = do
  cond <- eval cond
  case cond of
    Just (ScmBoolean True) -> eval t
    Just (ScmBoolean False) -> return Nothing
    Nothing -> throwE "Condition must evaluate to a boolean value!"
  
scmLet :: [ScmValue] -> ScmReturn (Maybe ScmValue)
scmLet ((ScmList binds):exprs) = do
  newEnv
  mapM scmBind binds
  results <- mapM eval exprs
  leaveEnv
  return $ last results
  where
    scmBind :: ScmValue -> ScmReturn ()
    scmBind (ScmList ((ScmSymbol key):val:_)) = do
      Just val <- eval val
      insertEnv key val
scmLet (ScmSymbol loop:(ScmList binds):exprs) = do
  newEnv
  kvs <- separate binds
  l@(ScmFunction lf) <- lambda ((ScmList $ map fst kvs):exprs)
  insertEnv loop l
  vals <- mapM eval (map snd kvs)
  r <- lf (catMaybes vals)
  leaveEnv
  return r
  where
    separate :: [ScmValue] -> ScmReturn [(ScmValue, ScmValue)]
    separate [] = return []
    separate ((ScmList [key, val]):cdr) = do
      cdr <- separate cdr
      return ((key,val):cdr)

lambda :: [ScmValue] -> ScmReturn ScmValue
lambda ((ScmList binds):body) = do
  binds <- mapM trySymbol binds
  defEnv <- get
  return $ ScmFunction (\args -> do
                                  callEnv <- get
                                  lift $ put defEnv
                                  newEnv
                                  bind binds args
                                  results <- mapM eval body
                                  lift $ put callEnv
                                  return $ last results)
  where
    bind :: [String] -> [ScmValue] -> ScmReturn ()
    bind [] [] = return ()
    bind [] vs = throwE ("Incorrect number of arguments to a function (" ++ concat (intersperse " " (map show vs)) ++ ")")
    bind ss [] = throwE "Missing arguments to a function"
    bind (s:ss) (v:vs) = insertEnv s v >> bind ss vs
lambda ((ScmPair binds rest):body) = do
  binds <- mapM trySymbol binds
  rest <- trySymbol rest
  defEnv <- get
  return $ ScmFunction (\args -> do
                                  callEnv <- get
                                  lift $ put defEnv
                                  newEnv
                                  bind binds rest args
                                  results <- mapM eval body
                                  lift $ put callEnv
                                  return $ last results)
  where
    bind :: [String] -> String -> [ScmValue] -> ScmReturn ()
    bind [] r vs = insertEnv r (ScmList vs)
    bind ss r [] = throwE "Missing arguments to a function"
    bind (s:ss) r (v:vs) = insertEnv s v >> bind ss r vs
lambda vs = throwE ("Incorrect arguments to lambda definition (" ++ concat (intersperse " " $ map show vs) ++ ")")
    
trySymbol :: ScmValue -> ScmReturn String
trySymbol (ScmSymbol s) = return s
trySymbol v = throwE ("Lambda binding must be a symbol, not a: `" ++ show v ++ "`")

define :: [ScmValue] -> ScmReturn ()
define [s@(ScmSymbol symbol),value] = do
    value <- eval value
    case value of
      Just v -> insertEnv symbol v
      Nothing -> throwE ("define `" ++ symbol ++ "` doesn't have a value!")
define ((ScmList (symbol@(ScmSymbol _):params)):value) = do
  function <- lambda ((ScmList (params)):value)
  define [symbol, function]
define ((ScmPair (symbol@(ScmSymbol _):params) rest):body) = do
  function <- lambda ((ScmPair (params) rest):body)
  define [symbol, function]
define e = throwE $ show e

set :: [ScmValue] -> ScmReturn ()
set [ScmSymbol symbol,value] = do
  value <- eval value
  oldValue <- getEnv symbol
  case (value, oldValue) of
    (_, Nothing) -> throwE ("Cannot set! symbol " ++ symbol ++ " because it has to be defined first!")
    (Nothing, _) -> throwE "set! expression must evaluate to a value"
    (Just value, Just _) -> insertEnv symbol value
    

load :: [ScmValue] -> ScmReturn ()
load [] = return ()
load xs = () <$ mapM loadFile xs
  where
    loadFile :: ScmValue -> ScmReturn ()
    loadFile (ScmString filename) = do
      file <- liftIO $ readFile filename
      exprs <- readMany file
      () <$ mapM eval exprs
    
eval :: ScmValue -> ScmReturn (Maybe ScmValue)
eval exp@(ScmList (car:cdr)) = do
  case car of
    ScmSymbol "eval" -> eval $ head cdr
    ScmSymbol "quote" -> case cdr of
      (caar:[]) -> return $ Just caar
      _ -> throwE "Cannot quote more than one argument"
    ScmSymbol "quasiquote" -> case cdr of
      (caar@(ScmList _):[]) -> Just . ScmList <$> quasiquote caar
      (other:[]) -> return $ Just other
      _ -> throwE "Cannot quasiquote more than one argument"
    ScmSymbol "define" -> Nothing <$ define cdr
    ScmSymbol "set!" -> Nothing <$ set cdr
    ScmSymbol "load" -> Nothing <$ load cdr
    ScmSymbol "lambda" -> Just <$> lambda cdr
    ScmSymbol "let" -> scmLet cdr
    ScmSymbol "if" -> scmIf cdr
    ScmSymbol "print" -> do
      v <- eval $ head cdr
      case v of
        Just v -> Nothing<$ scmDisplay v
        _ -> return Nothing
    ScmSymbol car -> do
      carV <- getEnv car
      case carV of
        Just (ScmFunction f) -> do
          cdr <- mapM eval cdr
          f (catMaybes cdr)
        Just v -> do
          scmDisplay exp
          throwE ("First element of a list must be a valid function! It is: " ++ show v)
        Nothing -> do
          env <- get
          throwE ("Symbol `" ++ car ++ "` is undefined in current context!")
    l@(ScmList _) -> do
      car <- eval l
      case car of
        Just car -> eval (ScmList (car:cdr))
        Nothing -> eval (ScmList cdr)
    ScmFunction f -> do
      cdr <- mapM eval cdr
      f (catMaybes cdr)
    v -> throwE ("First element of a list must be a function! It is: " ++ show v ++ " " ++ show cdr)
eval (ScmSymbol s) = do
  car <- getEnv s
  case car of
    Just v -> return $ Just v
    Nothing -> throwE $ "Symbol `" ++ s ++ "` undefined"
eval val = return $ Just val

re :: String -> ScmReturn (Maybe ScmValue)
re str = readStr str >>= eval

scmRead :: ScmValue -> ScmReturn ScmValue
scmRead (ScmString str) = readStr str

scmDisplay :: ScmValue -> ScmReturn ()
scmDisplay = liftIO . putStrLn . show 

scmPrint :: String -> ScmReturn ()
scmPrint = liftIO . putStrLn 
