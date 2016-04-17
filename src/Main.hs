module Main where

import Parser
import Syntax(Class)

import Control.Monad.Trans
import System.IO
import System.Environment
import System.Console.Haskeline

process :: String -> IO (Maybe [Class])
process source = do
  let res = parseCool source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> return $ Just ex

processFile :: String -> IO ()
processFile fname = do
  ast <- readFile fname >>= process
  print (show ast)

repl :: IO ()
repl = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "ready> "
          case minput of
            Nothing    -> outputStrLn "Goodbye."
            Just input -> do
              res <- liftIO $ process input
              loop

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname
