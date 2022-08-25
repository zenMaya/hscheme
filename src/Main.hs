{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.State (StateT, evalStateT, liftIO, get, put)
import Core
import Reader
import Repl
import System.Console.Haskeline
import System.Directory (getHomeDirectory, doesFileExist)
import System.IO.Error (tryIOError)
import Types
import Control.Monad.Trans.Class
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT), mapMaybeT)
import Control.Monad.Trans.Except (runExceptT, catchE)
import Control.Monad.State.Lazy (StateT(runStateT))
import qualified Data.Map.Strict as Map
import Data.IORef (readIORef)
import System.Environment (getArgs)
import Control.Monad

type ScmInteractive a = InputT (ExceptT String (StateT Env IO)) a

historyFilePath :: String -> String
historyFilePath = flip (++) "/.local/share/hscheme/history"

printBinds :: ScmInteractive ()
printBinds = do
      env :: Env <- lift get
      mapM_ (\e -> do
               env <- liftIO $ readIORef e
               mapM (\(k,v) -> liftIO $ putStrLn ("(" ++ k ++ " " ++ show v ++ ")")) $ Map.toList env)
        env

-- Read -> Eval -> Print -> Loop
repl :: ScmInteractive ()
repl = do
  line <- getInputLine "λ> "
  case line of
    Nothing -> return ()
    Just "" -> return ()
    Just ":binds" -> printBinds
    -- Read the input, parse it, evaluate it and print the result
    Just str -> lift $ catchE (do
                           v <- re str
                           case v of
                             Nothing -> return ()
                             Just v -> () <$ scmDisplay v) scmPrint
  repl

bind :: [(String, ScmValue)] -> ScmReturn ()
bind [] = return ()
bind ((name, val):binds) = insertEnv name val >> bind binds

-- Initialize the environment
init :: ScmInteractive ()
init = do
  lift $ bind prelude
  lift $ re "(load \"lib/Prelude.scm\")"
  repl
      
-- Run file
run :: String -> IO ()
run str = do
  rootEnv <- emptyEnv
  result <- evalStateT (runExceptT (bind prelude >> re "(load \"lib/Prelude.scm\")" >> re str)) rootEnv
  case result of
    Left e -> error e
    _ -> return ()

settings home = Settings {
  complete = completeFilename,
  historyFile = Just $ historyFilePath home,
  autoAddHistory = True
                    }

-- Run in interactive mode
interactive :: IO ()
interactive = do
  rootEnv <- emptyEnv
  home <- getHomeDirectory
  result <- evalStateT (runExceptT $ runInputT (settings home) Main.init) rootEnv
  case result of
    Left e -> putStrLn e
    _ -> return ()

-- If the executable was provided with arguments, treat them as source files and execute them
-- otherwise start interactive mode
menu :: [String] -> IO ()
menu [] = interactive
menu files = mapM_ (\file -> run ("(load \"" ++ file ++ "\")")) files

main :: IO ()
main = getArgs >>= menu
