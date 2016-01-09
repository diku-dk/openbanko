module Main (main) where

import Barc.Runner (runFromFile, runFromFileFuthark)

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Text.IO as T


showHelp :: IO ()
showHelp = do
  putStrLn "barc: compile board descriptions"
  putStrLn "usage:"
  putStrLn "  ./barc FILE.barc"

main :: IO ()
main = do
  args <- getArgs
  result <-
    case args of
    ["--gotta-go-fast", path] -> runFromFileFuthark path
    [path] -> runFromFile path
    _ -> showHelp >> exitFailure
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right s -> T.putStr s
