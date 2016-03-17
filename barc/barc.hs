module Main (main) where

import Barc.Runner (runFromFile, runFromFileBit, runFromFileFuthark)

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Text.IO as T


showHelp :: IO ()
showHelp = do
  putStrLn "barc: compile board descriptions"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  ./barc [OPTION] FILE.barc"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --gotta-go-fast  Compile to Futhark instead of C"
  putStrLn "  --fernando       Compile to NAND-only C instead of C (very slow)"

main :: IO ()
main = do
  args <- getArgs
  result <-
    case args of
    ["--help"] -> showHelp >> exitSuccess
    ["--gotta-go-fast", path] -> runFromFileFuthark path
    ["--fernando", path] -> runFromFileBit path
    [path] -> runFromFile path
    _ -> showHelp >> exitFailure
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right s -> T.putStr s
