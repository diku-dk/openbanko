module Main (main) where

import Control.Monad (when)

import Bankotrav.Formatting
import Bankotrav.Random
import Bankotrav.Compression


main :: IO ()
main = do
  putStrLn "Generating random board..."
  board <- randomBoardIO
  putStr $ formatBoard board
  putStrLn "Compressing board..."
  let idx = compressBoard board
  putStrLn ("Board index: " ++ show idx ++ "; binary: " ++ formatBinary idx)
  putStrLn "Decompressing board..."
  let board' = decompressBoard idx
  putStr $ formatBoard board'
  when (board /= board') $ putStrLn "BOARDS NOT EQUAL!"
