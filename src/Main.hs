module Main where
import Control.Monad.State (StateT, evalStateT, liftIO, get, put)
import Core
import Reader
import Repl
import qualified System.Console.Readline as Readline
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

historyFile :: IO String
historyFile = do
  home <- getHomeDirectory
  return $ home ++ "/.local/share/hscheme/history"

loadHistory :: IO ()
loadHistory = do
  h <- historyFile
  exists <- doesFileExist h
  when exists $ do
    c <- readFile h
    mapM_ Readline.addHistory (lines c)

addHistory :: String -> IO ()
addHistory line = do
  h <- historyFile
  _  <- tryIOError (appendFile h (line ++ "\n"))
  Readline.addHistory line

-- Read -> Eval -> Print -> Loop
repl :: ScmReturn ()
repl = do
  line <- liftIO $ Readline.readline "λ> "
  case line of
    Nothing -> return ()
    Just "" -> return ()
    -- Print current bound symbols and their values
    Just ":binds" -> do
      env <- get
      mapM_ (\e -> do
               env <- liftIO $ readIORef e
               mapM (\(k,v) -> liftIO $ putStrLn ("(" ++ k ++ " " ++ show v ++ ")")) $ Map.toList env)
        env
    -- Read the input, parse it, evaluate it and print the result
    Just str -> do
      liftIO $ addHistory str
      catchE (do
                           v <- re str
                           case v of
                             Nothing -> return ()
                             Just v -> () <$ scmDisplay v)
                scmPrint
  repl


-- Initialize the environment
init :: ScmReturn ()
init = do
  bind prelude
  re "(load \"lib/Prelude.scm\")"
  repl
    where
      bind :: [(String, ScmValue)] -> ScmReturn ()
      bind [] = return ()
      bind ((name, val):binds) = insertEnv name val >> bind binds

-- Run file
run :: String -> IO ()
run str = do
  rootEnv <- emptyEnv
  result <- evalStateT (runExceptT (bind prelude >> re "(load \"lib/Prelude.scm\")" >> re str)) rootEnv
  case result of
    Left e -> error e
    _ -> return ()
  where
    bind [] = return ()
    bind ((key, val):binds) = insertEnv key val >> bind binds

-- Run in interactive mode
interactive :: IO ()
interactive = do
  loadHistory
  rootEnv <- emptyEnv
  result <- evalStateT (runExceptT Main.init) rootEnv
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


