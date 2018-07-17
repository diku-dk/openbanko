-- bankotrav: traverse banko boards
module Main where

import Data.List (foldl', transpose, findIndex)
import Data.Maybe (fromMaybe)

import System.IO.Unsafe (unsafePerformIO)

import Bankotrav.Types
import Bankotrav.Base
import Bankotrav.Random
import Bankotrav.Formatting


nPossibleBoards :: BoardIncomplete -> Int
nPossibleBoards bi = sum $ map poss perms
  where perms = filter (columnPermCanWork bi) $ concatMap kindPerms allColumnSignaturePermutations
        poss :: ColumnBoardPerm -> Int
        poss perm = product $ zipWith colPoss perm [0..8]
        colPoss :: ColumnPerm -> Int -> Int
        colPoss (ka, kb, kc) col =
          let (a, b, c) = getColumn bi col
          in length $ work (minimumCellValue col - 1) (zip [a, b, c] [ka, kb, kc])
          where work _ [] = [[]]
                work prev ((t, tk) : us) = case tk of
                  Blank -> work prev us
                  Number -> case t of
                    FilledIn (ValueCell v) -> work v us
                    _ -> concatMap (\t -> work t us) [(prev + 1)..(maxk us)]

                maxk [] = maximumCellValue col
                maxk ((FilledIn (ValueCell t), _) : _) = t - 1
                maxk (_ : us) = maxk us


boardIndex :: Board -> Int
boardIndex board = fst $ foldl' step (0, emptyBoard) boardIndices
  where step (acc, bi) i = fromMaybe (error "impossible!") $ do
          cell <- getCell board i
          let choices = validCells bi i
          choice_i <- findIndex (== cell) choices
          let bi' = setCell bi i $ FilledIn cell
              acc' = acc + sum (map (nPossibleBoards . setCell bi i . FilledIn) (take choice_i choices))
          unsafePerformIO (print ("compress", i, acc, acc')) `seq` return (acc', bi')

indexToBoard :: Int -> Board
indexToBoard idx = fromIncomplete $ snd $ foldl' step (idx, emptyBoard) boardIndices
  where step (acc, bi) i =
          let choices = validCells bi i
              (choice, prev_sum) = find_choice 0 0 choices
              acc' = acc - prev_sum
              bi' = setCell bi i $ FilledIn choice
          in unsafePerformIO (print ("decompress", i, acc, acc')) `seq` (acc', bi')

          where find_choice cur_choice_i cur (choice : choices) =
                  let new = nPossibleBoards $ setCell bi i $ FilledIn choice
                      cur' = cur + new
                  in if cur' > acc
                  then (choice, cur)
                  else find_choice (cur_choice_i + 1) cur' choices
                find_choice _ _ [] = error "impossible!"


main :: IO ()
--main = putStr =<< formatBoard <$> randomBoardIO
main = do
  putStrLn "Generating random board..."
  board <- randomBoardIO
  putStr $ formatBoard board
  putStrLn "Compressing board..."
  let idx = boardIndex board
  print idx
  putStrLn $ formatBinary idx
  putStrLn "Decompressing board..."
  let board' = indexToBoard idx
  putStr $ formatBoard board'

  --print $ nPossibleBoards board
--  where -- board = setCell emptyBoard (0, 0) (FilledIn (ValueCell 3))
  --      board = emptyBoard
